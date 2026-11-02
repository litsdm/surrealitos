use std::fmt;

use serde::{Deserialize, Deserializer, Serialize, Serializer};
use surrealdb::{RecordId, sql::Id};

#[derive(Debug, Clone)]
pub struct SurrealId(pub RecordId);

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
                let parts: Vec<&str> = value.split(':').collect();
                if parts.len() != 2 {
                    return Err(serde::de::Error::custom(
                        "Invalid format for SurrealId: expected 'table:id'",
                    ));
                }
                let id = RecordId::from_table_key(parts[0], parts[1]);
                Ok(SurrealId(id))
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
        write!(f, "{}", self.0)
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
        let parts: Vec<&str> = s.split(':').collect();
        if parts.len() != 2 {
            return Err(SurrealIdParseError {
                input: s.to_string(),
            });
        }
        let id = RecordId::from_table_key(parts[0], parts[1]);
        Ok(SurrealId(id))
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
            ValueOrThing::Thing(thing) => serde_json::Value::String(thing.to_string()),
            ValueOrThing::Value(v) => v,
        }
    }
}

#[derive(Clone, Debug, Serialize)]
#[serde(untagged)]
pub enum Relation<T> {
    Id(SurrealId),
    Full(T),
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

            // There is an underlying issue with the crate serde-content on how they try to process enums
            // this issue affects surrealdb_core::sql::Id which in turn messes with this deserialization.
            // We had to patch serde and serde-content using @frederik-uni's patch, and implement this
            // custom deserializer to be able to deserialize Relations with nested SurrealIds or Relations
            // refs: [https://github.com/surrealdb/surrealdb/issues/4921#issuecomment-2539445295, https://github.com/rushmorem/serde-content/issues/27]
            fn visit_map<M>(self, mut map: M) -> Result<Self::Value, M::Error>
            where
                M: serde::de::MapAccess<'de>,
            {
                let key_string = map.next_key::<String>()?.unwrap_or("".to_string());
                let key = key_string.as_str();

                if key == "tb" || key == "id" {
                    let tb_value = map.next_value::<String>()?;

                    let _id_key = map.next_key::<&str>()?;
                    let id_value = map.next_value::<Id>()?;

                    let id = RecordId::from_table_key(tb_value, id_value.to_raw());
                    Ok(Relation::Id(SurrealId(id)))
                } else {
                    let first_value = map.next_value::<serde_json::Value>()?;

                    let mut entries = std::collections::BTreeMap::new();
                    entries.insert(key.to_string(), first_value);

                    while let Some((k, v)) = map.next_entry::<String, ValueOrThing>()? {
                        let value = v.into_json_value();
                        entries.insert(k, value);
                    }

                    let deserializer = serde::de::value::MapDeserializer::new(entries.into_iter());
                    let obj = T::deserialize(deserializer).map_err(serde::de::Error::custom)?;
                    Ok(Relation::Full(obj))
                }
            }
        }

        deserializer.deserialize_any(RelationVisitor(std::marker::PhantomData))
    }
}

pub fn extract_id(id: &str, sub_str: &str) -> String {
    let start = format!("{sub_str}:");
    let len = sub_str.len() + 1;
    match id.starts_with(&start) {
        true => id[len..].replace(['⟨', '⟩', '\\'], ""),
        false => id.to_string(),
    }
}
