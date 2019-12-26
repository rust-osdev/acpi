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

pub struct Namespace {
    /// This is a running count of ids, which are never reused. This is incremented every time we
    /// add a new object to the namespace. We can then remove objects, freeing their memory, without
    /// risking using the same id for two objects.
    next_handle: AmlHandle,

    /// This maps handles to actual values, and is used to access the actual AML values.
    object_map: BTreeMap<AmlHandle, AmlValue>,

    /// This maps names to handles, and should be used when you don't already have the handle to a
    /// value.
    // XXX: in the future, this could be replaced with a better data structure that doesn't store
    // the entire name for each id. Instead, it would be a tree-like structure that stores each
    // name segment, with a list of objects and their names, and a list of child scopes at that
    // level.
    name_map: BTreeMap<AmlName, AmlHandle>,
}

impl Namespace {
    pub fn new() -> Namespace {
        Namespace { next_handle: AmlHandle(0), object_map: BTreeMap::new(), name_map: BTreeMap::new() }
    }

    /// Add a value to the namespace at the given path, which must be a normalized, absolute AML
    /// name. If you want to add at a path relative to a given scope, use `add_at_resolved_path`
    /// instead.
    pub fn add(&mut self, path: AmlName, value: AmlValue) -> Result<AmlHandle, AmlError> {
        assert!(path.is_absolute());
        assert!(path.is_normal());

        if self.name_map.contains_key(&path) {
            return Err(AmlError::NameCollision(path.clone()));
        }

        let handle = self.next_handle;
        self.next_handle.increment();

        self.object_map.insert(handle, value);
        self.name_map.insert(path, handle);

        Ok(handle)
    }

    /// Helper method for adding a value to the namespace at a path that is relative to the given
    /// scope. This operation involves a lot of error handling in parts of the parser, so is
    /// encapsulated here.
    pub fn add_at_resolved_path(
        &mut self,
        path: AmlName,
        scope: &AmlName,
        value: AmlValue,
    ) -> Result<AmlHandle, AmlError> {
        self.add(path.resolve(scope)?, value)
    }

    pub fn get(&self, handle: AmlHandle) -> Result<&AmlValue, AmlError> {
        self.object_map.get(&handle).ok_or(AmlError::HandleDoesNotExist(handle))
    }

    pub fn get_by_path(&self, path: &AmlName) -> Result<&AmlValue, AmlError> {
        let handle = *self.name_map.get(path).ok_or(AmlError::ObjectDoesNotExist(path.as_string()))?;
        self.get(handle).map_err(|_| AmlError::ObjectDoesNotExist(path.as_string()))
    }

    pub fn get_mut(&mut self, handle: AmlHandle) -> Result<&mut AmlValue, AmlError> {
        self.object_map.get_mut(&handle).ok_or(AmlError::HandleDoesNotExist(handle))
    }

    pub fn get_by_path_mut(&mut self, path: &AmlName) -> Result<&mut AmlValue, AmlError> {
        let handle = *self.name_map.get(path).ok_or(AmlError::ObjectDoesNotExist(path.as_string()))?;
        self.get_mut(handle).map_err(|_| AmlError::ObjectDoesNotExist(path.as_string()))
    }

    /// Search for an object at the given path of the namespace, applying the search rules
    /// described in §5.3 of the ACPI specification, if they are applicable. Returns the resolved name, and the
    /// handle of the first valid object, if found.
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
                if let Some(handle) = self.name_map.get(&name) {
                    return Ok((name, *handle));
                }

                // If we don't find it, go up a level in the namespace and search for it there,
                // recursively.
                match scope.parent() {
                    Ok(parent) => scope = parent,
                    // If we still haven't found the value and have run out of parents, return `None`.
                    Err(AmlError::RootHasNoParent) => return Err(AmlError::ObjectDoesNotExist(path.as_string())),
                    Err(err) => return Err(err),
                }
            }
        } else {
            // If search rules don't apply, simply resolve it against the starting scope
            let name = path.resolve(starting_scope)?;
            Ok((
                name,
                self.name_map
                    .get(&path.resolve(starting_scope)?)
                    .map(|&handle| handle)
                    .ok_or(AmlError::ObjectDoesNotExist(path.as_string()))?,
            ))
        }
    }
}

impl fmt::Debug for Namespace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (name, handle) in self.name_map.iter() {
            write!(f, "{}: {:?}\n", name, self.object_map.get(handle).unwrap())?;
        }

        Ok(())
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct AmlName(pub(crate) Vec<NameComponent>);

impl AmlName {
    pub fn root() -> AmlName {
        AmlName(alloc::vec![NameComponent::Root])
    }

    pub fn from_name_seg(seg: NameSeg) -> AmlName {
        AmlName(alloc::vec![NameComponent::Segment(seg)])
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
        Ok(AmlName(self.0.iter().try_fold(alloc::vec![], |mut name, &component| match component {
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

#[cfg(test)]
mod tests {
    use super::*;

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
}
