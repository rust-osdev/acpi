use crate::{name_object::NameSeg, value::AmlValue, AmlError};
use alloc::{
    collections::BTreeMap,
    string::{String, ToString},
    vec::Vec,
};
use core::fmt;

/// A handle is used to refer to an AML value without actually borrowing it until you need to
/// access it (this makes borrowing situation much easier as you only have to consider who's
/// borrowing the namespace). They can also be cached to avoid expensive namespace lookups.
///
/// Handles are never reused (the handle to a removed object will never be reused to point to a new
/// object). This ensures handles cached by the library consumer will never point to an object they
/// did not originally point to, but also means that, in theory, we can run out of handles on a
/// very-long-running system (we are yet to see if this is a problem, practically).
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct AmlHandle(u32);

impl AmlHandle {
    pub(self) fn increment(&mut self) {
        self.0 += 1;
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum LevelType {
    Scope,
    Device,
    /// A legacy `Processor` object's sub-objects are stored in a level of this type. Modern tables define
    /// processors as `Device`s.
    Processor,
    /// A `PowerResource` object's sub-objects are stored in a level of this type.
    PowerResource,
    /// A `ThermalZone` object's sub-objects are stored in a level of this type.
    ThermalZone,
    /// A level of this type is created at the same path as the name of a method when it is invoked. It can be
    /// used by the method to store local variables.
    MethodLocals,
}

#[derive(Clone, Debug)]
pub struct NamespaceLevel {
    pub typ: LevelType,
    pub children: BTreeMap<NameSeg, NamespaceLevel>,
    pub values: BTreeMap<NameSeg, AmlHandle>,
}

impl NamespaceLevel {
    pub(crate) fn new(typ: LevelType) -> NamespaceLevel {
        NamespaceLevel { typ, children: BTreeMap::new(), values: BTreeMap::new() }
    }
}

#[derive(Clone)]
pub struct Namespace {
    /// This is a running count of ids, which are never reused. This is incremented every time we
    /// add a new object to the namespace. We can then remove objects, freeing their memory, without
    /// risking using the same id for two objects.
    next_handle: AmlHandle,

    /// This maps handles to actual values, and is used to access the actual AML values. When removing a value
    /// from the object map, care must be taken to also remove references to its handle in the level data
    /// structure, as invalid handles will cause panics.
    object_map: BTreeMap<AmlHandle, AmlValue>,

    /// Holds the first level of the namespace - containing items such as `\_SB`. Subsequent levels are held
    /// recursively inside this structure. It holds handles to references, which need to be indexed into
    /// `object_map` to acctually access the object.
    root: NamespaceLevel,
}

impl Namespace {
    pub fn new() -> Namespace {
        Namespace {
            next_handle: AmlHandle(0),
            object_map: BTreeMap::new(),
            root: NamespaceLevel::new(LevelType::Scope),
        }
    }

    /// Add a new level to the namespace. A "level" is named by a single `NameSeg`, and can contain values, and
    /// also other further sub-levels. Once a level has been created, AML values can be added to it with
    /// `add_value`.
    ///
    /// ### Note
    /// At first glance, you might expect `DefDevice` to add a value of type `Device`. However, because all
    /// `Devices` do is hold other values, we model them as namespace levels, and so they must be created
    /// accordingly.
    pub fn add_level(&mut self, path: AmlName, typ: LevelType) -> Result<(), AmlError> {
        assert!(path.is_absolute());
        let path = path.normalize()?;

        /*
         * We need to handle a special case here: if a `Scope(\) { ... }` appears in the AML, the parser will
         * try and recreate the root scope. Instead of handling this specially in the parser, we just
         * return nicely here.
         */
        if path != AmlName::root() {
            let (level, last_seg) = self.get_level_for_path_mut(&path)?;

            /*
             * If the level has already been added, we don't need to add it again. The parser can try to add it
             * multiple times if the ASL contains multiple blocks that add to the same scope/device.
             */
            if !level.children.contains_key(&last_seg) {
                level.children.insert(last_seg, NamespaceLevel::new(typ));
            }
        }

        Ok(())
    }

    pub fn remove_level(&mut self, path: AmlName) -> Result<(), AmlError> {
        assert!(path.is_absolute());
        let path = path.normalize()?;

        if path != AmlName::root() {
            let (level, last_seg) = self.get_level_for_path_mut(&path)?;

            match level.children.remove(&last_seg) {
                Some(_) => Ok(()),
                None => Err(AmlError::LevelDoesNotExist(path)),
            }
        } else {
            Err(AmlError::TriedToRemoveRootNamespace)
        }
    }

    /// Add a value to the namespace at the given path, which must be a normalized, absolute AML
    /// name. If you want to add at a path relative to a given scope, use `add_at_resolved_path`
    /// instead.
    pub fn add_value(&mut self, path: AmlName, value: AmlValue) -> Result<AmlHandle, AmlError> {
        assert!(path.is_absolute());
        let path = path.normalize()?;

        let handle = self.next_handle;
        self.next_handle.increment();
        self.object_map.insert(handle, value);

        let (level, last_seg) = self.get_level_for_path_mut(&path)?;
        match level.values.insert(last_seg, handle) {
            None => Ok(handle),
            Some(_) => Err(AmlError::NameCollision(path)),
        }
    }

    /// Helper method for adding a value to the namespace at a path that is relative to the given
    /// scope. This operation involves a lot of error handling in parts of the parser, so is
    /// encapsulated here.
    pub fn add_value_at_resolved_path(
        &mut self,
        path: AmlName,
        scope: &AmlName,
        value: AmlValue,
    ) -> Result<AmlHandle, AmlError> {
        self.add_value(path.resolve(scope)?, value)
    }

    pub fn get(&self, handle: AmlHandle) -> Result<&AmlValue, AmlError> {
        Ok(self.object_map.get(&handle).unwrap())
    }

    pub fn get_mut(&mut self, handle: AmlHandle) -> Result<&mut AmlValue, AmlError> {
        Ok(self.object_map.get_mut(&handle).unwrap())
    }

    pub fn get_handle(&self, path: &AmlName) -> Result<AmlHandle, AmlError> {
        let (level, last_seg) = self.get_level_for_path(path)?;
        Ok(*level.values.get(&last_seg).ok_or(AmlError::ValueDoesNotExist(path.clone()))?)
    }

    pub fn get_by_path(&self, path: &AmlName) -> Result<&AmlValue, AmlError> {
        let handle = self.get_handle(path)?;
        Ok(self.get(handle).unwrap())
    }

    pub fn get_by_path_mut(&mut self, path: &AmlName) -> Result<&mut AmlValue, AmlError> {
        let handle = self.get_handle(path)?;
        Ok(self.get_mut(handle).unwrap())
    }

    /// Search for an object at the given path of the namespace, applying the search rules described in §5.3 of the
    /// ACPI specification, if they are applicable. Returns the resolved name, and the handle of the first valid
    /// object, if found.
    pub fn search(&self, path: &AmlName, starting_scope: &AmlName) -> Result<(AmlName, AmlHandle), AmlError> {
        if path.search_rules_apply() {
            /*
             * If search rules apply, we need to recursively look through the namespace. If the
             * given name does not occur in the current scope, we look at the parent scope, until
             * we either find the name, or reach the root of the namespace.
             */
            let mut scope = starting_scope.clone();
            assert!(scope.is_absolute());
            loop {
                // Search for the name at this namespace level. If we find it, we're done.
                let name = path.resolve(&scope)?;
                match self.get_level_for_path(&name) {
                    Ok((level, last_seg)) => {
                        if let Some(&handle) = level.values.get(&last_seg) {
                            return Ok((name, handle));
                        }
                    }

                    /*
                     * This error is caught specially to avoid a case that seems bizzare but is quite useful - when
                     * the passed starting scope doesn't exist. Certain methods return values that reference names
                     * from the point of view of the method, so it makes sense for the starting scope to be inside
                     * the method.  However, because we have destroyed all the objects created by the method
                     * dynamically, the level no longer exists.
                     *
                     * To avoid erroring here, we simply continue to the parent scope. If the whole scope doesn't
                     * exist, this will error when we get to the root, so this seems unlikely to introduce bugs.
                     */
                    Err(AmlError::LevelDoesNotExist(_)) => (),
                    Err(err) => return Err(err),
                }

                // If we don't find it, go up a level in the namespace and search for it there recursively
                match scope.parent() {
                    Ok(parent) => scope = parent,
                    // If we still haven't found the value and have run out of parents, return `None`.
                    Err(AmlError::RootHasNoParent) => return Err(AmlError::ValueDoesNotExist(path.clone())),
                    Err(err) => return Err(err),
                }
            }
        } else {
            // If search rules don't apply, simply resolve it against the starting scope
            let name = path.resolve(starting_scope)?;
            // TODO: the fuzzer crashes when path is `\` and the scope is also `\`. This means that name is `\`,
            // which then trips up get_level_for_path. I don't know where to best solve this: we could check for
            // specific things that crash `search`, or look for a more general solution.
            let (level, last_seg) = self.get_level_for_path(&path.resolve(starting_scope)?)?;

            if let Some(&handle) = level.values.get(&last_seg) {
                Ok((name, handle))
            } else {
                Err(AmlError::ValueDoesNotExist(path.clone()))
            }
        }
    }

    pub fn search_for_level(&self, level_name: &AmlName, starting_scope: &AmlName) -> Result<AmlName, AmlError> {
        if level_name.search_rules_apply() {
            let mut scope = starting_scope.clone().normalize()?;
            assert!(scope.is_absolute());

            loop {
                let name = level_name.resolve(&scope)?;
                if let Ok((level, last_seg)) = self.get_level_for_path(&name) {
                    if let Some(_) = level.children.get(&last_seg) {
                        return Ok(name);
                    }
                }

                // If we don't find it, move the scope up a level and search for it there recursively
                match scope.parent() {
                    Ok(parent) => scope = parent,
                    Err(AmlError::RootHasNoParent) => return Err(AmlError::LevelDoesNotExist(level_name.clone())),
                    Err(err) => return Err(err),
                }
            }
        } else {
            Ok(level_name.clone())
        }
    }

    fn get_level_for_path(&self, path: &AmlName) -> Result<(&NamespaceLevel, NameSeg), AmlError> {
        assert_ne!(*path, AmlName::root());

        let (last_seg, levels) = path.0[1..].split_last().unwrap();
        let last_seg = last_seg.as_segment().unwrap();

        // TODO: this helps with diagnostics, but requires a heap allocation just in case we need to error.
        let mut traversed_path = AmlName::root();

        let mut current_level = &self.root;
        for level in levels {
            traversed_path.0.push(*level);
            current_level = current_level
                .children
                .get(&level.as_segment().unwrap())
                .ok_or(AmlError::LevelDoesNotExist(traversed_path.clone()))?;
        }

        Ok((current_level, last_seg))
    }

    /// Split an absolute path into a bunch of level segments (used to traverse the level data structure), and a
    /// last segment to index into that level. This must not be called on `\\`.
    fn get_level_for_path_mut(&mut self, path: &AmlName) -> Result<(&mut NamespaceLevel, NameSeg), AmlError> {
        assert_ne!(*path, AmlName::root());

        let (last_seg, levels) = path.0[1..].split_last().unwrap();
        let last_seg = last_seg.as_segment().unwrap();

        // TODO: this helps with diagnostics, but requires a heap allocation just in case we need to error. We can
        // improve this by changing the `levels` interation into an `enumerate()`, and then using the index to
        // create the correct path on the error path
        let mut traversed_path = AmlName::root();

        let mut current_level = &mut self.root;
        for level in levels {
            traversed_path.0.push(*level);
            current_level = current_level
                .children
                .get_mut(&level.as_segment().unwrap())
                .ok_or(AmlError::LevelDoesNotExist(traversed_path.clone()))?;
        }

        Ok((current_level, last_seg))
    }

    /// Traverse the namespace, calling `f` on each namespace level. `f` returns a `Result<bool, AmlError>` -
    /// errors terminate the traversal and are propagated, and the `bool` on the successful path marks whether the
    /// children of the level should also be traversed.
    pub fn traverse<F>(&mut self, mut f: F) -> Result<(), AmlError>
    where
        F: FnMut(&AmlName, &NamespaceLevel) -> Result<bool, AmlError>,
    {
        fn traverse_level<F>(level: &NamespaceLevel, scope: &AmlName, f: &mut F) -> Result<(), AmlError>
        where
            F: FnMut(&AmlName, &NamespaceLevel) -> Result<bool, AmlError>,
        {
            for (name, ref child) in level.children.iter() {
                let name = AmlName::from_name_seg(*name).resolve(scope)?;

                if f(&name, child)? {
                    traverse_level(child, &name, f)?;
                }
            }

            Ok(())
        }

        if f(&AmlName::root(), &self.root)? {
            traverse_level(&self.root, &AmlName::root(), &mut f)?;
        }

        Ok(())
    }
}

impl fmt::Debug for Namespace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        const INDENT_PER_LEVEL: usize = 4;

        fn print_level(
            namespace: &Namespace,
            f: &mut fmt::Formatter<'_>,
            level_name: &str,
            level: &NamespaceLevel,
            indent: usize,
        ) -> fmt::Result {
            writeln!(f, "{:indent$}{}:", "", level_name, indent = indent)?;

            for (name, handle) in level.values.iter() {
                writeln!(
                    f,
                    "{:indent$}{}: {:?}",
                    "",
                    name.as_str(),
                    namespace.object_map.get(handle).unwrap(),
                    indent = indent + INDENT_PER_LEVEL
                )?;
            }

            for (name, sub_level) in level.children.iter() {
                print_level(namespace, f, name.as_str(), sub_level, indent + INDENT_PER_LEVEL)?;
            }

            Ok(())
        }

        print_level(self, f, "\\", &self.root, 0)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct AmlName(Vec<NameComponent>);

impl AmlName {
    pub fn root() -> AmlName {
        AmlName(alloc::vec![NameComponent::Root])
    }

    pub fn from_name_seg(seg: NameSeg) -> AmlName {
        AmlName(alloc::vec![NameComponent::Segment(seg)])
    }

    pub fn from_components(components: Vec<NameComponent>) -> AmlName {
        assert!(components.len() > 0);
        AmlName(components)
    }

    /// Convert a string representation of an AML name into an `AmlName`.
    pub fn from_str(mut string: &str) -> Result<AmlName, AmlError> {
        if string.len() == 0 {
            return Err(AmlError::EmptyNamesAreInvalid);
        }

        let mut components = Vec::new();

        // If it starts with a \, make it an absolute name
        if string.starts_with('\\') {
            components.push(NameComponent::Root);
            string = &string[1..];
        }

        if string.len() > 0 {
            // Divide the rest of it into segments, and parse those
            for mut part in string.split('.') {
                // Handle prefix chars
                while part.starts_with('^') {
                    components.push(NameComponent::Prefix);
                    part = &part[1..];
                }

                components.push(NameComponent::Segment(NameSeg::from_str(part)?));
            }
        }

        Ok(AmlName(components))
    }

    pub fn as_string(&self) -> String {
        self.0
            .iter()
            .fold(String::new(), |name, component| match component {
                NameComponent::Root => name + "\\",
                NameComponent::Prefix => name + "^",
                NameComponent::Segment(seg) => name + seg.as_str() + ".",
            })
            .trim_end_matches('.')
            .to_string()
    }

    /// An AML path is normal if it does not contain any prefix elements ("^" characters, when
    /// expressed as a string).
    pub fn is_normal(&self) -> bool {
        !self.0.contains(&NameComponent::Prefix)
    }

    pub fn is_absolute(&self) -> bool {
        self.0.first() == Some(&NameComponent::Root)
    }

    /// Special rules apply when searching for certain paths (specifically, those that are made up
    /// of a single name segment). Returns `true` if those rules apply.
    pub fn search_rules_apply(&self) -> bool {
        if self.0.len() != 1 {
            return false;
        }

        match self.0[0] {
            NameComponent::Segment(_) => true,
            _ => false,
        }
    }

    /// Normalize an AML path, resolving prefix chars. Returns `AmlError::InvalidNormalizedName` if the path
    /// normalizes to an invalid path (e.g. `\^_FOO`)
    pub fn normalize(self) -> Result<AmlName, AmlError> {
        /*
         * If the path is already normal, just return it as-is. This avoids an unneccessary heap allocation and
         * free.
         */
        if self.is_normal() {
            return Ok(self);
        }

        Ok(AmlName(self.0.iter().try_fold(Vec::new(), |mut name, &component| match component {
            seg @ NameComponent::Segment(_) => {
                name.push(seg);
                Ok(name)
            }

            NameComponent::Root => {
                name.push(NameComponent::Root);
                Ok(name)
            }

            NameComponent::Prefix => {
                if let Some(NameComponent::Segment(_)) = name.iter().last() {
                    name.pop().unwrap();
                    Ok(name)
                } else {
                    Err(AmlError::InvalidNormalizedName(self.clone()))
                }
            }
        })?))
    }

    /// Get the parent of this `AmlName`. For example, the parent of `\_SB.PCI0._PRT` is `\_SB.PCI0`. The root
    /// path has no parent, and so returns `None`.
    pub fn parent(&self) -> Result<AmlName, AmlError> {
        // Firstly, normalize the path so we don't have to deal with prefix chars
        let mut normalized_self = self.clone().normalize()?;

        match normalized_self.0.last() {
            None | Some(NameComponent::Root) => Err(AmlError::RootHasNoParent),
            Some(NameComponent::Segment(_)) => {
                normalized_self.0.pop();
                Ok(normalized_self)
            }
            Some(NameComponent::Prefix) => unreachable!(), // Prefix chars are removed by normalization
        }
    }

    /// Resolve this path against a given scope, making it absolute. If the path is absolute, it is
    /// returned directly. The path is also normalized.
    pub fn resolve(&self, scope: &AmlName) -> Result<AmlName, AmlError> {
        assert!(scope.is_absolute());

        if self.is_absolute() {
            return Ok(self.clone());
        }

        let mut resolved_path = scope.clone();
        resolved_path.0.extend_from_slice(&(self.0));
        resolved_path.normalize()
    }
}

impl fmt::Display for AmlName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_string())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum NameComponent {
    Root,
    Prefix,
    Segment(NameSeg),
}

impl NameComponent {
    pub fn as_segment(self) -> Result<NameSeg, ()> {
        match self {
            NameComponent::Segment(seg) => Ok(seg),
            NameComponent::Root | NameComponent::Prefix => Err(()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::crudely_cmp_values;

    #[test]
    fn test_aml_name_from_str() {
        assert_eq!(AmlName::from_str(""), Err(AmlError::EmptyNamesAreInvalid));
        assert_eq!(AmlName::from_str("\\"), Ok(AmlName::root()));
        assert_eq!(
            AmlName::from_str("\\_SB.PCI0"),
            Ok(AmlName(alloc::vec![
                NameComponent::Root,
                NameComponent::Segment(NameSeg([b'_', b'S', b'B', b'_'])),
                NameComponent::Segment(NameSeg([b'P', b'C', b'I', b'0']))
            ]))
        );
        assert_eq!(
            AmlName::from_str("\\_SB.^^^PCI0"),
            Ok(AmlName(alloc::vec![
                NameComponent::Root,
                NameComponent::Segment(NameSeg([b'_', b'S', b'B', b'_'])),
                NameComponent::Prefix,
                NameComponent::Prefix,
                NameComponent::Prefix,
                NameComponent::Segment(NameSeg([b'P', b'C', b'I', b'0']))
            ]))
        );
    }

    #[test]
    fn test_is_normal() {
        assert_eq!(AmlName::root().is_normal(), true);
        assert_eq!(AmlName::from_str("\\_SB.PCI0.VGA").unwrap().is_normal(), true);
        assert_eq!(AmlName::from_str("\\_SB.^PCI0.VGA").unwrap().is_normal(), false);
        assert_eq!(AmlName::from_str("\\^_SB.^^PCI0.VGA").unwrap().is_normal(), false);
        assert_eq!(AmlName::from_str("_SB.^^PCI0.VGA").unwrap().is_normal(), false);
        assert_eq!(AmlName::from_str("_SB.PCI0.VGA").unwrap().is_normal(), true);
    }

    #[test]
    fn test_normalization() {
        assert_eq!(
            AmlName::from_str("\\_SB.PCI0").unwrap().normalize(),
            Ok(AmlName::from_str("\\_SB.PCI0").unwrap())
        );
        assert_eq!(
            AmlName::from_str("\\_SB.^PCI0").unwrap().normalize(),
            Ok(AmlName::from_str("\\PCI0").unwrap())
        );
        assert_eq!(
            AmlName::from_str("\\_SB.PCI0.^^FOO").unwrap().normalize(),
            Ok(AmlName::from_str("\\FOO").unwrap())
        );
        assert_eq!(
            AmlName::from_str("_SB.PCI0.^FOO.BAR").unwrap().normalize(),
            Ok(AmlName::from_str("_SB.FOO.BAR").unwrap())
        );
        assert_eq!(
            AmlName::from_str("\\^_SB").unwrap().normalize(),
            Err(AmlError::InvalidNormalizedName(AmlName::from_str("\\^_SB").unwrap()))
        );
        assert_eq!(
            AmlName::from_str("\\_SB.PCI0.FOO.^^^^BAR").unwrap().normalize(),
            Err(AmlError::InvalidNormalizedName(AmlName::from_str("\\_SB.PCI0.FOO.^^^^BAR").unwrap()))
        );
    }

    #[test]
    fn test_is_absolute() {
        assert_eq!(AmlName::root().is_absolute(), true);
        assert_eq!(AmlName::from_str("\\_SB.PCI0.VGA").unwrap().is_absolute(), true);
        assert_eq!(AmlName::from_str("\\_SB.^PCI0.VGA").unwrap().is_absolute(), true);
        assert_eq!(AmlName::from_str("\\^_SB.^^PCI0.VGA").unwrap().is_absolute(), true);
        assert_eq!(AmlName::from_str("_SB.^^PCI0.VGA").unwrap().is_absolute(), false);
        assert_eq!(AmlName::from_str("_SB.PCI0.VGA").unwrap().is_absolute(), false);
    }

    #[test]
    fn test_search_rules_apply() {
        assert_eq!(AmlName::root().search_rules_apply(), false);
        assert_eq!(AmlName::from_str("\\_SB").unwrap().search_rules_apply(), false);
        assert_eq!(AmlName::from_str("^VGA").unwrap().search_rules_apply(), false);
        assert_eq!(AmlName::from_str("_SB.PCI0.VGA").unwrap().search_rules_apply(), false);
        assert_eq!(AmlName::from_str("VGA").unwrap().search_rules_apply(), true);
        assert_eq!(AmlName::from_str("_SB").unwrap().search_rules_apply(), true);
    }

    #[test]
    fn test_aml_name_parent() {
        assert_eq!(AmlName::from_str("\\").unwrap().parent(), Err(AmlError::RootHasNoParent));
        assert_eq!(AmlName::from_str("\\_SB").unwrap().parent(), Ok(AmlName::root()));
        assert_eq!(AmlName::from_str("\\_SB.PCI0").unwrap().parent(), Ok(AmlName::from_str("\\_SB").unwrap()));
        assert_eq!(AmlName::from_str("\\_SB.PCI0").unwrap().parent().unwrap().parent(), Ok(AmlName::root()));
    }

    #[test]
    fn test_namespace() {
        let mut namespace = Namespace::new();

        /*
         * This should succeed but do nothing.
         */
        assert_eq!(namespace.add_level(AmlName::from_str("\\").unwrap(), LevelType::Scope), Ok(()));

        /*
         * Add `\_SB`, also testing that adding a level twice succeeds.
         */
        assert_eq!(namespace.add_level(AmlName::from_str("\\_SB").unwrap(), LevelType::Scope), Ok(()));
        assert_eq!(namespace.add_level(AmlName::from_str("\\_SB").unwrap(), LevelType::Scope), Ok(()));

        /*
         * Add a device under a level that already exists.
         */
        assert_eq!(namespace.add_level(AmlName::from_str("\\_SB.PCI0").unwrap(), LevelType::Device), Ok(()));

        /*
         * Add some deeper scopes.
         */
        assert_eq!(namespace.add_level(AmlName::from_str("\\FOO").unwrap(), LevelType::Scope), Ok(()));
        assert_eq!(namespace.add_level(AmlName::from_str("\\FOO.BAR").unwrap(), LevelType::Scope), Ok(()));
        assert_eq!(namespace.add_level(AmlName::from_str("\\FOO.BAR.BAZ").unwrap(), LevelType::Scope), Ok(()));
        assert_eq!(namespace.add_level(AmlName::from_str("\\FOO.BAR.BAZ").unwrap(), LevelType::Scope), Ok(()));
        assert_eq!(namespace.add_level(AmlName::from_str("\\FOO.BAR.BAZ.QUX").unwrap(), LevelType::Scope), Ok(()));

        /*
         * Add some things to the scopes to query later.
         */
        assert!(namespace.add_value(AmlName::from_str("\\MOO").unwrap(), AmlValue::Boolean(true)).is_ok());
        assert!(namespace.add_value(AmlName::from_str("\\FOO.BAR.A").unwrap(), AmlValue::Integer(12345)).is_ok());
        assert!(namespace.add_value(AmlName::from_str("\\FOO.BAR.B").unwrap(), AmlValue::Integer(6)).is_ok());
        assert!(namespace
            .add_value(AmlName::from_str("\\FOO.BAR.C").unwrap(), AmlValue::String(String::from("hello, world!")))
            .is_ok());

        /*
         * Get objects using their absolute paths.
         */
        assert!(crudely_cmp_values(
            namespace.get_by_path(&AmlName::from_str("\\MOO").unwrap()).unwrap(),
            &AmlValue::Boolean(true)
        ));
        assert!(crudely_cmp_values(
            namespace.get_by_path(&AmlName::from_str("\\FOO.BAR.A").unwrap()).unwrap(),
            &AmlValue::Integer(12345)
        ));
        assert!(crudely_cmp_values(
            namespace.get_by_path(&AmlName::from_str("\\FOO.BAR.B").unwrap()).unwrap(),
            &AmlValue::Integer(6)
        ));
        assert!(crudely_cmp_values(
            namespace.get_by_path(&AmlName::from_str("\\FOO.BAR.C").unwrap()).unwrap(),
            &AmlValue::String(String::from("hello, world!"))
        ));

        /*
         * Search for some objects that should use search rules.
         */
        {
            let (name, _) = namespace
                .search(&AmlName::from_str("MOO").unwrap(), &AmlName::from_str("\\FOO.BAR.BAZ").unwrap())
                .unwrap();
            assert_eq!(name, AmlName::from_str("\\MOO").unwrap());
        }
        {
            let (name, _) = namespace
                .search(&AmlName::from_str("A").unwrap(), &AmlName::from_str("\\FOO.BAR").unwrap())
                .unwrap();
            assert_eq!(name, AmlName::from_str("\\FOO.BAR.A").unwrap());
        }
        {
            let (name, _) = namespace
                .search(&AmlName::from_str("A").unwrap(), &AmlName::from_str("\\FOO.BAR.BAZ.QUX").unwrap())
                .unwrap();
            assert_eq!(name, AmlName::from_str("\\FOO.BAR.A").unwrap());
        }
    }

    #[test]
    fn test_get_level_for_path() {
        let mut namespace = Namespace::new();

        // Add some scopes
        assert_eq!(namespace.add_level(AmlName::from_str("\\FOO").unwrap(), LevelType::Scope), Ok(()));
        assert_eq!(namespace.add_level(AmlName::from_str("\\FOO.BAR").unwrap(), LevelType::Scope), Ok(()));
        assert_eq!(namespace.add_level(AmlName::from_str("\\FOO.BAR.BAZ").unwrap(), LevelType::Scope), Ok(()));
        assert_eq!(namespace.add_level(AmlName::from_str("\\FOO.BAR.BAZ").unwrap(), LevelType::Scope), Ok(()));
        assert_eq!(namespace.add_level(AmlName::from_str("\\FOO.BAR.BAZ.QUX").unwrap(), LevelType::Scope), Ok(()));

        {
            let (_, last_seg) =
                namespace.get_level_for_path(&AmlName::from_str("\\FOO.BAR.BAZ").unwrap()).unwrap();
            assert_eq!(last_seg, NameSeg::from_str("BAZ").unwrap());
        }
        {
            let (_, last_seg) = namespace.get_level_for_path(&AmlName::from_str("\\FOO").unwrap()).unwrap();
            assert_eq!(last_seg, NameSeg::from_str("FOO").unwrap());
        }
    }
}
