use super::object::WrappedObject;
use crate::aml::AmlError;
use alloc::{
    collections::btree_map::BTreeMap,
    string::{String, ToString},
    vec,
    vec::Vec,
};
use bit_field::BitField;
use core::{fmt, str, str::FromStr};
use log::trace;

#[derive(Clone)]
pub struct Namespace {
    root: NamespaceLevel,
}

impl Namespace {
    /// Create a new AML namespace, with the expected pre-defined objects.
    pub fn new() -> Namespace {
        let mut namespace = Namespace { root: NamespaceLevel::new(NamespaceLevelKind::Scope) };

        namespace.add_level(AmlName::from_str("\\_GPE").unwrap(), NamespaceLevelKind::Scope).unwrap();
        namespace.add_level(AmlName::from_str("\\_SB").unwrap(), NamespaceLevelKind::Scope).unwrap();
        namespace.add_level(AmlName::from_str("\\_SI").unwrap(), NamespaceLevelKind::Scope).unwrap();
        namespace.add_level(AmlName::from_str("\\_PR").unwrap(), NamespaceLevelKind::Scope).unwrap();
        namespace.add_level(AmlName::from_str("\\_TZ").unwrap(), NamespaceLevelKind::Scope).unwrap();

        // TODO: add pre-defined objects as well - \GL, \OSI, etc.

        namespace
    }

    pub fn add_level(&mut self, path: AmlName, kind: NamespaceLevelKind) -> Result<(), AmlError> {
        assert!(path.is_absolute());
        let path = path.normalize()?;

        // Don't try to recreate the root scope
        if path != AmlName::root() {
            let (level, last_seg) = self.get_level_for_path_mut(&path)?;

            /*
             * If the level has already been added, we don't need to add it again. The parser can try to add it
             * multiple times if the ASL contains multiple blocks that add to the same scope/device.
             */
            level.children.entry(last_seg).or_insert_with(|| NamespaceLevel::new(kind));
        }

        Ok(())
    }

    pub fn remove_level(&mut self, path: AmlName) -> Result<(), AmlError> {
        assert!(path.is_absolute());
        let path = path.normalize()?;

        // Don't try to remove the root scope
        // TODO: we probably shouldn't be able to remove the pre-defined scopes either?
        if path != AmlName::root() {
            let (level, last_seg) = self.get_level_for_path_mut(&path)?;
            level.children.remove(&last_seg);
        }

        Ok(())
    }

    pub fn insert(&mut self, path: AmlName, object: WrappedObject) -> Result<(), AmlError> {
        assert!(path.is_absolute());
        let path = path.normalize()?;

        let (level, last_seg) = self.get_level_for_path_mut(&path)?;
        match level.values.insert(last_seg, (ObjectFlags::new(false), object)) {
            None => Ok(()),
            Some(_) => {
                /*
                 * Real AML often has name collisions, and so we can't afford to be too strict
                 * about it. We do warn the user as it does have the potential to break stuff.
                 */
                trace!("AML name collision: {}. Replacing object.", path);
                Ok(())
            }
        }
    }

    pub fn create_alias(&mut self, path: AmlName, object: WrappedObject) -> Result<(), AmlError> {
        assert!(path.is_absolute());
        let path = path.normalize()?;

        let (level, last_seg) = self.get_level_for_path_mut(&path)?;
        match level.values.insert(last_seg, (ObjectFlags::new(true), object)) {
            None => Ok(()),
            Some(_) => Err(AmlError::NameCollision(path)),
        }
    }

    pub fn get(&mut self, path: AmlName) -> Result<WrappedObject, AmlError> {
        assert!(path.is_absolute());
        let path = path.normalize()?;

        let (level, last_seg) = self.get_level_for_path_mut(&path)?;
        match level.values.get(&last_seg) {
            Some((_, object)) => Ok(object.clone()),
            None => Err(AmlError::ObjectDoesNotExist(path.clone())),
        }
    }

    /// Search for an object at the given path of the namespace, applying the search rules described in §5.3 of the
    /// ACPI specification, if they are applicable. Returns the resolved name, and the handle of the first valid
    /// object, if found.
    pub fn search(&self, path: &AmlName, starting_scope: &AmlName) -> Result<(AmlName, WrappedObject), AmlError> {
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
                        if let Some((_, object)) = level.values.get(&last_seg) {
                            return Ok((name, object.clone()));
                        }
                    }

                    Err(err) => return Err(err),
                }

                // If we don't find it, go up a level in the namespace and search for it there recursively
                match scope.parent() {
                    Ok(parent) => scope = parent,
                    Err(AmlError::RootHasNoParent) => return Err(AmlError::ObjectDoesNotExist(path.clone())),
                    Err(err) => return Err(err),
                }
            }
        } else {
            // If search rules don't apply, simply resolve it against the starting scope
            let name = path.resolve(starting_scope)?;
            let (level, last_seg) = self.get_level_for_path(&path.resolve(starting_scope)?)?;

            if let Some((_, object)) = level.values.get(&last_seg) {
                Ok((name, object.clone()))
            } else {
                Err(AmlError::ObjectDoesNotExist(path.clone()))
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
                    if level.children.contains_key(&last_seg) {
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

    /// Split an absolute path into a bunch of level segments (used to traverse the level data structure), and a
    /// last segment to index into that level. This must not be called on `\\`.
    fn get_level_for_path(&self, path: &AmlName) -> Result<(&NamespaceLevel, NameSeg), AmlError> {
        assert_ne!(*path, AmlName::root());

        let (last_seg, levels) = path.0[1..].split_last().unwrap();
        let NameComponent::Segment(last_seg) = last_seg else {
            panic!();
        };

        // TODO: this helps with diagnostics, but requires a heap allocation just in case we need to error.
        let mut traversed_path = AmlName::root();

        let mut current_level = &self.root;
        for level in levels {
            traversed_path.0.push(*level);

            let NameComponent::Segment(segment) = level else {
                panic!();
            };
            current_level =
                current_level.children.get(&segment).ok_or(AmlError::LevelDoesNotExist(traversed_path.clone()))?;
        }

        Ok((current_level, *last_seg))
    }

    /// Split an absolute path into a bunch of level segments (used to traverse the level data structure), and a
    /// last segment to index into that level. This must not be called on `\\`.
    fn get_level_for_path_mut(&mut self, path: &AmlName) -> Result<(&mut NamespaceLevel, NameSeg), AmlError> {
        assert_ne!(*path, AmlName::root());

        let (last_seg, levels) = path.0[1..].split_last().unwrap();
        let NameComponent::Segment(last_seg) = last_seg else {
            panic!();
        };

        // TODO: this helps with diagnostics, but requires a heap allocation just in case we need to error. We can
        // improve this by changing the `levels` interation into an `enumerate()`, and then using the index to
        // create the correct path on the error path
        let mut traversed_path = AmlName::root();

        let mut current_level = &mut self.root;
        for level in levels {
            traversed_path.0.push(*level);

            let NameComponent::Segment(segment) = level else {
                panic!();
            };
            current_level = current_level
                .children
                .get_mut(&segment)
                .ok_or(AmlError::LevelDoesNotExist(traversed_path.clone()))?;
        }

        Ok((current_level, *last_seg))
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
            for (name, child) in level.children.iter() {
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

impl fmt::Display for Namespace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        const STEM: &str = "│   ";
        const BRANCH: &str = "├── ";
        const END: &str = "└── ";

        fn print_level(
            namespace: &Namespace,
            f: &mut fmt::Formatter<'_>,
            level: &NamespaceLevel,
            indent_stack: String,
        ) -> fmt::Result {
            for (i, (name, (flags, object))) in level.values.iter().enumerate() {
                let end = (i == level.values.len() - 1)
                    && level.children.iter().filter(|(_, l)| l.kind == NamespaceLevelKind::Scope).count() == 0;
                writeln!(
                    f,
                    "{}{}{}: {}{}",
                    &indent_stack,
                    if end { END } else { BRANCH },
                    name.as_str(),
                    if flags.is_alias() { "[A] " } else { "" },
                    **object
                )?;

                // If the object has a corresponding scope, print it here
                if let Some(child_level) = level.children.get(&name) {
                    print_level(
                        namespace,
                        f,
                        child_level,
                        if end { indent_stack.clone() + "    " } else { indent_stack.clone() + STEM },
                    )?;
                }
            }

            let remaining_scopes: Vec<_> =
                level.children.iter().filter(|(_, l)| l.kind == NamespaceLevelKind::Scope).collect();
            for (i, (name, sub_level)) in remaining_scopes.iter().enumerate() {
                let end = i == remaining_scopes.len() - 1;
                writeln!(f, "{}{}{}:", &indent_stack, if end { END } else { BRANCH }, name.as_str())?;
                print_level(namespace, f, sub_level, indent_stack.clone() + STEM)?;
            }

            Ok(())
        }

        writeln!(f, "\n    \\:")?;
        print_level(self, f, &self.root, String::from("    "))
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum NamespaceLevelKind {
    Scope,
    Device,
    Processor,
    PowerResource,
    ThermalZone,
    MethodLocals,
}

#[derive(Clone)]
pub struct NamespaceLevel {
    pub kind: NamespaceLevelKind,
    pub values: BTreeMap<NameSeg, (ObjectFlags, WrappedObject)>,
    pub children: BTreeMap<NameSeg, NamespaceLevel>,
}

#[derive(Clone, Copy, Debug)]
pub struct ObjectFlags(u8);

impl ObjectFlags {
    pub fn new(is_alias: bool) -> ObjectFlags {
        let mut flags = 0;
        flags.set_bit(0, is_alias);
        ObjectFlags(flags)
    }

    pub fn is_alias(&self) -> bool {
        self.0.get_bit(0)
    }
}

impl NamespaceLevel {
    pub fn new(kind: NamespaceLevelKind) -> NamespaceLevel {
        NamespaceLevel { kind, values: BTreeMap::new(), children: BTreeMap::new() }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct AmlName(Vec<NameComponent>);

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum NameComponent {
    Root,
    Prefix,
    Segment(NameSeg),
}

impl AmlName {
    pub fn root() -> AmlName {
        AmlName(vec![NameComponent::Root])
    }

    pub fn from_name_seg(seg: NameSeg) -> AmlName {
        AmlName(vec![NameComponent::Segment(seg)])
    }

    pub fn from_components(components: Vec<NameComponent>) -> AmlName {
        AmlName(components)
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

        matches!(self.0[0], NameComponent::Segment(_))
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

impl FromStr for AmlName {
    type Err = AmlError;

    fn from_str(mut string: &str) -> Result<Self, Self::Err> {
        if string.is_empty() {
            return Err(AmlError::EmptyNamesAreInvalid);
        }

        let mut components = Vec::new();

        // If it starts with a \, make it an absolute name
        if string.starts_with('\\') {
            components.push(NameComponent::Root);
            string = &string[1..];
        }

        if !string.is_empty() {
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

        Ok(Self(components))
    }
}

impl fmt::Display for AmlName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_string())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct NameSeg(pub(crate) [u8; 4]);

impl NameSeg {
    pub fn from_str(string: &str) -> Result<NameSeg, AmlError> {
        // Each NameSeg can only have four chars, and must have at least one
        if string.is_empty() || string.len() > 4 {
            return Err(AmlError::InvalidNameSeg([0xff, 0xff, 0xff, 0xff]));
        }

        // We pre-fill the array with '_', so it will already be correct if the length is < 4
        let mut seg = [b'_'; 4];
        let bytes = string.as_bytes();

        // Manually do the first one, because we have to check it's a LeadNameChar
        if !is_lead_name_char(bytes[0]) {
            return Err(AmlError::InvalidNameSeg([bytes[0], bytes[1], bytes[2], bytes[3]]));
        }
        seg[0] = bytes[0];

        // Copy the rest of the chars, checking that they're NameChars
        for i in 1..bytes.len() {
            if !is_name_char(bytes[i]) {
                return Err(AmlError::InvalidNameSeg([bytes[0], bytes[1], bytes[2], bytes[3]]));
            }
            seg[i] = bytes[i];
        }

        Ok(NameSeg(seg))
    }

    pub fn from_bytes(bytes: [u8; 4]) -> Result<NameSeg, AmlError> {
        if !is_lead_name_char(bytes[0]) {
            return Err(AmlError::InvalidNameSeg(bytes));
        }
        if !is_name_char(bytes[1]) {
            return Err(AmlError::InvalidNameSeg(bytes));
        }
        if !is_name_char(bytes[2]) {
            return Err(AmlError::InvalidNameSeg(bytes));
        }
        if !is_name_char(bytes[3]) {
            return Err(AmlError::InvalidNameSeg(bytes));
        }
        Ok(NameSeg(bytes))
    }

    pub fn as_str(&self) -> &str {
        // We should only construct valid ASCII name segments
        unsafe { str::from_utf8_unchecked(&self.0) }
    }
}

pub fn is_lead_name_char(c: u8) -> bool {
    c.is_ascii_uppercase() || c == b'_'
}

pub fn is_name_char(c: u8) -> bool {
    is_lead_name_char(c) || c.is_ascii_digit()
}

impl fmt::Debug for NameSeg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.as_str())
    }
}
