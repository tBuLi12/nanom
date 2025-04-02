use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::{self, BufWriter, Write},
    mem,
    path::Path,
};

use crate::TsType;

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct TypeRef {
    pub get_type: fn() -> Type,
}

#[derive(Clone)]
pub enum Type {
    Null,
    Boolean,
    Number,
    BigInt,
    String,
    NamedObject {
        id: TypeRef,
        name: String,
        fields: HashMap<String, TypeRef>,
    },
    Object(HashMap<String, TypeRef>),
    Enum {
        id: TypeRef,
        name: String,
        kinds: HashMap<String, HashMap<String, TypeRef>>,
    },
    Function {
        args: Vec<TypeRef>,
        return_type: Box<TypeRef>,
    },
    Promise(Box<TypeRef>),
    Array(Box<TypeRef>),
    Optional(Box<TypeRef>),
    Tuple(Vec<TypeRef>),
    Undefined,
    DataView,
}

pub struct Undefined;

impl TsType for Undefined {
    fn ts_type() -> Type {
        Type::Undefined
    }
}

pub fn generate_tds_file<Module: TsType>(
    _module: Module,
    out_path: impl AsRef<Path>,
) -> io::Result<()> {
    let writer = BufWriter::new(File::create(out_path)?);

    let Type::Object(fields) = Module::ts_type() else {
        panic!("Module must be an object");
    };

    let mut writer = Writer {
        out: writer,
        defined_types_ids: HashSet::new(),
        defined_types: HashMap::new(),
    };

    for (name, item) in fields {
        write!(writer.out, "export const {name}: ")?;
        writer.write_ts_type(&(item.get_type)())?;
        writeln!(writer.out, ";")?;
    }

    writer.write_definitions()?;

    Ok(())
}

struct Writer {
    out: BufWriter<File>,
    defined_types_ids: HashSet<TypeRef>,
    defined_types: HashMap<String, Type>,
}

impl Writer {
    pub fn define_type(&mut self, name: &str, id: TypeRef, ts_type: &Type) {
        if self.defined_types_ids.contains(&id) {
            return;
        }

        if self.defined_types.contains_key(name) {
            panic!("Type {name} is already defined");
        }

        self.defined_types_ids.insert(id);
        self.defined_types.insert(name.to_string(), ts_type.clone());
    }

    pub fn write_ts_type(&mut self, ts_type: &Type) -> io::Result<()> {
        match ts_type {
            Type::Null => write!(self.out, "null"),
            Type::Boolean => write!(self.out, "boolean"),
            Type::Number => write!(self.out, "number"),
            Type::BigInt => write!(self.out, "bigint"),
            Type::String => write!(self.out, "string"),
            Type::Object(fields) => self.write_ts_object(&fields),
            Type::NamedObject { id, name, .. } => {
                write!(self.out, "{name}")?;

                self.define_type(&name, *id, ts_type);

                Ok(())
            }
            Type::Enum { id, name, .. } => {
                write!(self.out, "{name}")?;

                self.define_type(&name, *id, ts_type);

                Ok(())
            }
            Type::Function { args, return_type } => {
                write!(self.out, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    write!(self.out, "arg{i}: ")?;
                    self.write_ts_type(&(arg.get_type)())?;
                    if i != args.len() - 1 {
                        write!(self.out, ", ")?;
                    }
                }
                write!(self.out, ") => ")?;
                self.write_ts_type(&(return_type.get_type)())
            }
            Type::Array(elem) => {
                write!(self.out, "(")?;
                self.write_ts_type(&(elem.get_type)())?;
                write!(self.out, ")[]")
            }
            Type::Optional(inner) => {
                write!(self.out, "(")?;
                self.write_ts_type(&(inner.get_type)())?;
                write!(self.out, ") | null")
            }
            Type::Tuple(inner) => {
                write!(self.out, "[")?;
                for (i, ty) in inner.iter().enumerate() {
                    self.write_ts_type(&(ty.get_type)())?;
                    if i != inner.len() - 1 {
                        write!(self.out, ", ")?;
                    }
                }
                write!(self.out, "]")
            }
            Type::Promise(inner) => {
                write!(self.out, "Promise<")?;
                self.write_ts_type(&(inner.get_type)())?;
                write!(self.out, ">")
            }
            Type::Undefined => write!(self.out, "undefined"),
            Type::DataView => write!(self.out, "DataView"),
        }
    }

    pub fn write_ts_object(&mut self, fields: &HashMap<String, TypeRef>) -> io::Result<()> {
        write!(self.out, "{{")?;
        for (name, field) in fields {
            write!(self.out, "{name}: ")?;
            self.write_ts_type(&(field.get_type)())?;
            write!(self.out, ", ")?;
        }
        write!(self.out, "}}")
    }

    pub fn write_definitions(&mut self) -> io::Result<()> {
        while !self.defined_types.is_empty() {
            for (name, ts_type) in mem::take(&mut self.defined_types) {
                write!(self.out, "export type {name} = ")?;
                match ts_type {
                    Type::NamedObject { fields, .. } => {
                        self.write_ts_object(&fields)?;
                    }
                    Type::Enum { kinds, .. } => {
                        for (name, fields) in kinds {
                            write!(self.out, "| {{ kind: \"{name}\", fields: ")?;
                            self.write_ts_object(&fields)?;
                            write!(self.out, " }}")?;
                        }
                    }
                    _ => unreachable!(),
                };
                writeln!(self.out, ";")?;
            }
        }

        Ok(())
    }
}
