use std::str::FromStr;

use acpi::{
    Handle,
    aml::{
        AmlError,
        namespace::{AmlName, Namespace, NamespaceLevelKind},
        object::Object,
    },
};

#[test]
fn namespace_rejects_relative_paths_without_panicking() {
    let mut namespace = Namespace::new(Handle(0));
    let relative = AmlName::from_str("_STA").unwrap();
    let expected = Err(AmlError::NameNotAbsolute(relative.clone()));

    assert_eq!(namespace.add_level(relative.clone(), NamespaceLevelKind::Scope), expected);
    assert_eq!(namespace.remove_level(relative.clone()), expected);
    assert_eq!(namespace.insert(relative.clone(), Object::Integer(0).wrap()), expected);
    assert_eq!(namespace.create_alias(relative.clone(), Object::Integer(0).wrap()), expected);
    assert_eq!(namespace.get(relative.clone()).err(), Some(AmlError::NameNotAbsolute(relative)));
}

#[test]
fn namespace_searches_reject_relative_starting_scopes_without_panicking() {
    let namespace = Namespace::new(Handle(0));
    let name = AmlName::from_str("_STA").unwrap();
    let relative_scope = AmlName::from_str("DEV0").unwrap();
    let expected = AmlError::NameNotAbsolute(relative_scope.clone());

    assert_eq!(namespace.search(&name, &relative_scope).map(|(name, _)| name), Err(expected.clone()));
    assert_eq!(namespace.search_for_level(&name, &relative_scope), Err(expected));
}

#[test]
fn relative_name_resolution_rejects_relative_scope_without_panicking() {
    let relative_name = AmlName::from_str("_STA").unwrap();
    let relative_scope = AmlName::from_str("DEV0").unwrap();

    assert_eq!(relative_name.resolve(&relative_scope), Err(AmlError::NameNotAbsolute(relative_scope.clone())));

    // An absolute name still requires an absolute scope, as the scope is validated first.
    let absolute_name = AmlName::from_str("\\_SB.DEV0._STA").unwrap();
    assert_eq!(absolute_name.resolve(&relative_scope), Err(AmlError::NameNotAbsolute(relative_scope)));
}

#[test]
fn absolute_paths_are_still_accepted() {
    let mut namespace = Namespace::new(Handle(0));
    let absolute = AmlName::from_str("\\DEV0").unwrap();

    namespace.insert(absolute.clone(), Object::Integer(7).wrap()).unwrap();
    assert!(matches!(*namespace.get(absolute).unwrap(), Object::Integer(7)));

    let name = AmlName::from_str("_STA").unwrap();
    assert_eq!(
        name.resolve(&AmlName::from_str("\\_SB.DEV0").unwrap()),
        Ok(AmlName::from_str("\\_SB.DEV0._STA").unwrap())
    );
}
