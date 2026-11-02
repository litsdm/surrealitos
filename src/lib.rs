use std::fmt;

use serde::{Deserialize, Deserializer, Serialize, Serializer};
use surrealdb::types::{
    ConversionError, Error, Kind, Object, RecordId, RecordIdKey, SurrealValue, ToSql, Value,
};

#[derive(Debug, Clone)]
pub struct SurrealId(pub RecordId);

// Written by hand rather than derived so that `is_value` only accepts a record
// link (or an id-shaped string). `RecordId`'s own `is_value` also accepts any
// object carrying `table`/`key` fields, which would make `Relation<T>` decode a
// fetched, fully-populated record as a bare id.
impl SurrealValue for SurrealId {
    fn kind_of() -> Kind {
        RecordId::kind_of()
    }

    fn is_value(value: &Value) -> bool {
        match value {
            Value::RecordId(_) => true,
            Value::String(s) => RecordId::parse_simple(s).is_ok(),
            _ => false,
        }
    }

    fn into_value(self) -> Value {
        Value::RecordId(self.0)
    }

    fn from_value(value: Value) -> Result<Self, Error> {
        match value {
            Value::RecordId(record_id) => Ok(SurrealId(record_id)),
            Value::String(s) => RecordId::parse_simple(&s)
                .map(SurrealId)
                .map_err(|err| Error::internal(err.to_string())),
            other => Err(ConversionError::from_value(Self::kind_of(), &other).into()),
        }
    }
}

impl<'de> Deserialize<'de> for SurrealId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct SurrealIdVisitor;

        impl<'de> serde::de::Visitor<'de> for SurrealIdVisitor {
            type Value = SurrealId;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a string in the format 'table:id' or a RecordId object")
            }

            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                value.parse().map_err(serde::de::Error::custom)
            }

            fn visit_map<M>(self, map: M) -> Result<Self::Value, M::Error>
            where
                M: serde::de::MapAccess<'de>,
            {
                let id = RecordId::deserialize(serde::de::value::MapAccessDeserializer::new(map))?;
                Ok(SurrealId(id))
            }
        }

        deserializer.deserialize_any(SurrealIdVisitor)
    }
}

impl Serialize for SurrealId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.to_string().serialize(serializer)
    }
}

impl fmt::Display for SurrealId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.to_sql())
    }
}

#[derive(Debug, thiserror::Error)]
#[error("Invalid SurrealId format: expected 'table:id', got '{input}'")]
pub struct SurrealIdParseError {
    input: String,
}

impl std::str::FromStr for SurrealId {
    type Err = SurrealIdParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (table, key) = s.split_once(':').ok_or_else(|| SurrealIdParseError {
            input: s.to_string(),
        })?;

        if table.is_empty() || key.is_empty() {
            return Err(SurrealIdParseError {
                input: s.to_string(),
            });
        }

        Ok(SurrealId(RecordId::new(table, unescape_key(key))))
    }
}

impl From<&str> for SurrealId {
    fn from(s: &str) -> Self {
        s.parse().expect("Invalid SurrealId format")
    }
}

impl From<String> for SurrealId {
    fn from(s: String) -> Self {
        s.parse().expect("Invalid SurrealId format")
    }
}

impl From<SurrealId> for RecordId {
    fn from(id: SurrealId) -> Self {
        id.0
    }
}

impl From<RecordId> for SurrealId {
    fn from(id: RecordId) -> Self {
        SurrealId(id)
    }
}

pub fn serialize_as_record<S>(
    id: &SurrealId,
    serializer: S,
) -> core::result::Result<S::Ok, S::Error>
where
    S: Serializer,
{
    id.0.serialize(serializer)
}

pub fn serialize_as_optional_record<S>(
    id: &Option<SurrealId>,
    serializer: S,
) -> core::result::Result<S::Ok, S::Error>
where
    S: Serializer,
{
    match id {
        Some(ser_id) => Some(ser_id.clone().0).serialize(serializer),
        None => id.serialize(serializer),
    }
}

#[allow(dead_code)]
pub fn serialize_as_record_vec<S>(
    ids: &[SurrealId],
    serializer: S,
) -> core::result::Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let record_ids: Vec<RecordId> = ids.iter().map(|id| id.0.clone()).collect();
    record_ids.serialize(serializer)
}

pub fn serialize_as_optional_record_vec<S>(
    ids: &Option<Vec<SurrealId>>,
    serializer: S,
) -> core::result::Result<S::Ok, S::Error>
where
    S: Serializer,
{
    match ids {
        Some(vec_ids) => {
            let record_ids: Vec<RecordId> = vec_ids.iter().map(|id| id.0.clone()).collect();
            Some(record_ids).serialize(serializer)
        }
        None => ids.serialize(serializer),
    }
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum ValueOrThing {
    Thing(RecordId),
    Value(serde_json::Value),
}

impl ValueOrThing {
    fn into_json_value(self) -> serde_json::Value {
        match self {
            ValueOrThing::Thing(thing) => serde_json::Value::String(thing.to_sql()),
            ValueOrThing::Value(v) => v,
        }
    }
}

/// A field that holds either a bare record link or the record it points at,
/// so the same struct decodes a plain `SELECT` and a `SELECT ... FETCH` alike.
///
/// Nest it as deep as the fetch goes: `Relation<Post>` where `Post` itself has
/// a `Relation<Author>` resolves both levels when both are fetched, and stays
/// `Id` at whichever level was not.
#[derive(Clone, Debug, Serialize)]
#[serde(untagged)]
pub enum Relation<T> {
    Id(SurrealId),
    Full(T),
}

impl<T> Relation<T> {
    pub fn id(&self) -> Option<&SurrealId> {
        match self {
            Relation::Id(id) => Some(id),
            Relation::Full(_) => None,
        }
    }

    pub fn full(&self) -> Option<&T> {
        match self {
            Relation::Id(_) => None,
            Relation::Full(record) => Some(record),
        }
    }
}

// The bound lives on the impl rather than on `Relation` itself so that a
// `Relation<T>` over a serde-only `T` is still a usable type.
impl<T> SurrealValue for Relation<T>
where
    T: SurrealValue,
{
    fn kind_of() -> Kind {
        Kind::Either(vec![SurrealId::kind_of(), T::kind_of()])
    }

    fn is_value(value: &Value) -> bool {
        SurrealId::is_value(value) || T::is_value(value)
    }

    fn into_value(self) -> Value {
        match self {
            Relation::Id(id) => id.into_value(),
            Relation::Full(record) => record.into_value(),
        }
    }

    // Order matters: a record link has to be recognised before `T` gets a look
    // at it, or a `T` whose fields are all optional would swallow it.
    fn from_value(value: Value) -> Result<Self, Error> {
        if SurrealId::is_value(&value) {
            return SurrealId::from_value(value).map(Relation::Id);
        }

        T::from_value(value).map(Relation::Full)
    }
}

impl<'de, T> Deserialize<'de> for Relation<T>
where
    T: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct RelationVisitor<T>(std::marker::PhantomData<T>);

        impl<'de, T> serde::de::Visitor<'de> for RelationVisitor<T>
        where
            T: Deserialize<'de>,
        {
            type Value = Relation<T>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a string ID or an object")
            }

            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Relation::Id(SurrealId::from(value)))
            }

            // A record id reaches a serde visitor as a map keyed by `table` and
            // `key`. The backing object is a BTreeMap, so `key` arrives first,
            // but both orders are handled. Anything else is a full `T`.
            fn visit_map<M>(self, mut map: M) -> Result<Self::Value, M::Error>
            where
                M: serde::de::MapAccess<'de>,
            {
                let key_string = map.next_key::<String>()?.unwrap_or_default();
                let key = key_string.as_str();

                if key == "table" || key == "key" {
                    let (table, record_key) = if key == "table" {
                        let table = map.next_value::<String>()?;
                        let _key_key = map.next_key::<String>()?;
                        let record_key = map.next_value::<RecordIdKey>()?;
                        (table, record_key)
                    } else {
                        let record_key = map.next_value::<RecordIdKey>()?;
                        let _table_key = map.next_key::<String>()?;
                        let table = map.next_value::<String>()?;
                        (table, record_key)
                    };

                    return Ok(Relation::Id(SurrealId(RecordId::new(table, record_key))));
                }

                let first_value = map.next_value::<serde_json::Value>()?;

                let mut entries = std::collections::BTreeMap::new();
                entries.insert(key.to_string(), first_value);

                while let Some((k, v)) = map.next_entry::<String, ValueOrThing>()? {
                    entries.insert(k, v.into_json_value());
                }

                let deserializer = serde::de::value::MapDeserializer::new(entries.into_iter());
                let obj = T::deserialize(deserializer).map_err(serde::de::Error::custom)?;
                Ok(Relation::Full(obj))
            }
        }

        deserializer.deserialize_any(RelationVisitor(std::marker::PhantomData))
    }
}

/// Drops every `NONE` entry from an object, recursively.
///
/// Arrays are left alone: their elements are values in their own right, not
/// absent fields.
pub fn prune_none(value: Value) -> Value {
    let Value::Object(object) = value else {
        return value;
    };

    let mut pruned = Object::new();
    for (key, entry) in object.into_inner() {
        if matches!(entry, Value::None) {
            continue;
        }

        pruned.insert(key, prune_none(entry));
    }

    Value::Object(pruned)
}

/// A write payload whose `None` fields are left out instead of written as
/// `NONE`.
///
/// `SurrealValue` emits every field it is derived on, so an `Option::None` on a
/// patch struct reaches `MERGE` as `NONE` and clears the stored field. Wrapping
/// the payload drops those keys, leaving a merge to touch only the fields the
/// patch actually carries:
///
/// ```ignore
/// db.update(("peer", key)).merge(Patch(peer_patch)).await?;
/// ```
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Patch<T>(pub T);

impl<T> Patch<T> {
    pub fn into_inner(self) -> T {
        self.0
    }
}

impl<T> SurrealValue for Patch<T>
where
    T: SurrealValue,
{
    fn kind_of() -> Kind {
        T::kind_of()
    }

    fn is_value(value: &Value) -> bool {
        T::is_value(value)
    }

    fn into_value(self) -> Value {
        prune_none(self.0.into_value())
    }

    fn from_value(value: Value) -> Result<Self, Error> {
        T::from_value(value).map(Patch)
    }
}

/// Strips a leading `table:` and the `⟨⟩` SurrealQL escapes from a rendered
/// record id, leaving the bare key.
pub fn extract_id(id: &str, sub_str: &str) -> String {
    let start = format!("{sub_str}:");
    let len = sub_str.len() + 1;
    match id.starts_with(&start) {
        true => unescape_key(&id[len..]),
        false => id.to_string(),
    }
}

fn unescape_key(key: &str) -> String {
    key.replace(['⟨', '⟩', '\\'], "")
}

#[cfg(test)]
mod tests {
    use surrealdb::types::{Object, SurrealValue, Value};

    use super::*;

    #[derive(Clone, Debug, Deserialize, Serialize, SurrealValue)]
    struct Author {
        id: SurrealId,
        name: String,
    }

    #[derive(Clone, Debug, Deserialize, Serialize, SurrealValue)]
    struct Post {
        id: SurrealId,
        title: String,
        author: Relation<Author>,
    }

    #[derive(Clone, Debug, Default, Serialize, SurrealValue)]
    struct PostPatch {
        title: Option<String>,
        author: Option<SurrealId>,
    }

    fn object(entries: impl IntoIterator<Item = (&'static str, Value)>) -> Value {
        let mut object = Object::new();
        for (key, value) in entries {
            object.insert(key, value);
        }

        Value::Object(object)
    }

    fn author_value() -> Value {
        object([
            ("id", SurrealId::from("author:ada").into_value()),
            ("name", Value::String("Ada".to_string())),
        ])
    }

    #[test]
    fn surreal_id_round_trips_through_a_string() {
        let id: SurrealId = "user:abc123".parse().unwrap();

        assert_eq!(id.to_string(), "user:abc123");
        assert_eq!(id.0.table.as_str(), "user");
    }

    #[test]
    fn surreal_id_rejects_input_without_a_table_and_key() {
        assert!("userabc".parse::<SurrealId>().is_err());
        assert!("user:".parse::<SurrealId>().is_err());
        assert!(":abc".parse::<SurrealId>().is_err());
    }

    #[test]
    fn surreal_id_parses_an_escaped_key() {
        let id: SurrealId = "user:⟨01HXYZ⟩".parse().unwrap();

        assert_eq!(extract_id(&id.to_string(), "user"), "01HXYZ");
    }

    #[test]
    fn surreal_id_reads_a_record_link_or_an_id_string() {
        let from_link = SurrealId::from_value(SurrealId::from("user:abc").into_value()).unwrap();
        let from_string = SurrealId::from_value(Value::String("user:abc".to_string())).unwrap();

        assert_eq!(from_link.to_string(), "user:abc");
        assert_eq!(from_string.to_string(), "user:abc");
        assert!(SurrealId::from_value(Value::Bool(true)).is_err());
    }

    #[test]
    fn surreal_id_serializes_to_a_string_for_the_frontend() {
        let id = SurrealId::from("user:abc");

        assert_eq!(serde_json::to_string(&id).unwrap(), "\"user:abc\"");
        assert_eq!(
            serde_json::from_str::<SurrealId>("\"user:abc\"")
                .unwrap()
                .to_string(),
            "user:abc"
        );
    }

    /// An unfetched link decodes as `Id`, the same field fetched decodes as
    /// `Full` — that is the whole point of the wrapper.
    #[test]
    fn relation_reads_a_bare_link_as_an_id() {
        let relation = Relation::<Author>::from_value(SurrealId::from("author:ada").into_value())
            .expect("a record link is a relation");

        assert_eq!(relation.id().unwrap().to_string(), "author:ada");
        assert!(relation.full().is_none());
    }

    #[test]
    fn relation_reads_a_fetched_record_as_the_full_value() {
        let relation =
            Relation::<Author>::from_value(author_value()).expect("an object is a relation");

        assert_eq!(relation.full().unwrap().name, "Ada");
        assert!(relation.id().is_none());
    }

    /// The nesting case the crate exists for: an outer relation that is
    /// fetched, holding an inner one that is not.
    #[test]
    fn relation_nests_as_deep_as_the_fetch_goes() {
        let post = object([
            ("id", SurrealId::from("post:1").into_value()),
            ("title", Value::String("Notes".to_string())),
            ("author", SurrealId::from("author:ada").into_value()),
        ]);

        let relation = Relation::<Post>::from_value(post).expect("an object is a relation");
        let post = relation.full().expect("the outer relation was fetched");

        assert_eq!(post.title, "Notes");
        assert_eq!(post.author.id().unwrap().to_string(), "author:ada");
    }

    #[test]
    fn relation_nests_when_every_level_is_fetched() {
        let post = object([
            ("id", SurrealId::from("post:1").into_value()),
            ("title", Value::String("Notes".to_string())),
            ("author", author_value()),
        ]);

        let relation = Relation::<Post>::from_value(post).expect("an object is a relation");
        let post = relation.full().expect("the outer relation was fetched");

        assert_eq!(post.author.full().unwrap().name, "Ada");
    }

    #[test]
    fn relation_writes_back_the_shape_it_holds() {
        let id = Relation::<Author>::Id(SurrealId::from("author:ada")).into_value();
        assert!(matches!(id, Value::RecordId(_)));

        let full = Relation::<Author>::from_value(author_value())
            .unwrap()
            .into_value();
        assert!(matches!(full, Value::Object(_)));
    }

    #[test]
    fn patch_leaves_out_the_fields_that_are_none() {
        let patch = Patch(PostPatch {
            title: Some("Notes".to_string()),
            author: None,
        });

        let Value::Object(object) = patch.into_value() else {
            panic!("a patch is an object");
        };

        assert!(object.contains_key("title"));
        assert!(!object.contains_key("author"));
    }

    #[test]
    fn patch_prunes_nested_objects_too() {
        let nested = object([
            ("kept", Value::String("yes".to_string())),
            ("dropped", Value::None),
        ]);
        let value = object([("shortcuts", nested), ("name", Value::None)]);

        let Value::Object(object) = prune_none(value) else {
            panic!("pruning an object gives an object");
        };

        assert!(!object.contains_key("name"));
        let Some(Value::Object(shortcuts)) = object.get("shortcuts") else {
            panic!("the nested object survives");
        };
        assert!(shortcuts.contains_key("kept"));
        assert!(!shortcuts.contains_key("dropped"));
    }

    /// Array elements are values, not absent fields, so they are left alone.
    #[test]
    fn patch_leaves_array_elements_alone() {
        let value = object([(
            "members",
            Value::Array(vec![Value::None, Value::String("ada".to_string())].into()),
        )]);

        let Value::Object(object) = prune_none(value) else {
            panic!("pruning an object gives an object");
        };
        let Some(Value::Array(members)) = object.get("members") else {
            panic!("the array survives");
        };

        assert_eq!(members.len(), 2);
    }

    /// Queries compare against plain strings (`trust_level != 'blocked'`), so a
    /// unit enum variant has to reach the db as one. Without `untagged` the
    /// derive writes `{ "blocked": {} }` instead and every such comparison
    /// silently stops matching.
    #[test]
    fn untagged_unit_enum_variants_are_stored_as_plain_strings() {
        #[derive(Debug, PartialEq, SurrealValue)]
        #[surreal(untagged, rename_all = "camelCase")]
        enum TrustLevel {
            Untrusted,
            Blocked,
        }

        assert_eq!(
            TrustLevel::Blocked.into_value(),
            Value::String("blocked".to_string())
        );
        assert_eq!(
            TrustLevel::from_value(Value::String("untrusted".to_string())).unwrap(),
            TrustLevel::Untrusted
        );
        assert!(TrustLevel::from_value(Value::String("nope".to_string())).is_err());
    }

    #[test]
    fn extract_id_strips_the_table_prefix_only_when_it_is_there() {
        assert_eq!(extract_id("user:abc", "user"), "abc");
        assert_eq!(extract_id("abc", "user"), "abc");
        assert_eq!(extract_id("peer:abc", "user"), "peer:abc");
    }
}
