use std::collections::HashMap;
use std::ops::{Add, Sub};
use pest::iterators::Pair;

use crate::parser::{Rule, SyntaxNode};

pub struct Interpreter {
    objects: Vec<Object>,
    classes: Vec<Class>,
}

#[derive(Copy, Clone, Debug)]
pub struct ObjectId(u64);

#[derive(Copy, Clone, Debug)]
pub struct ClassId(u64);

pub struct Object {
    id: ObjectId,
    class: ClassId,
    fields: HashMap<String, ObjectId>,
}

pub enum MethodReceiver {
    Class,
    Instance,
}

pub struct Method {
    name: String,
    arity: u8,
    receiver: MethodReceiver,
    body: MethodBody,
}

pub enum MethodBody {
    User(SyntaxNode),
}

pub struct Class {
    id: ClassId,
    name: String,
    methods: Vec<Method>,
}


impl Interpreter {
    pub fn new() -> Self {
        Self {
            objects: Default::default(),
            classes: Default::default(),
        }
    }

    fn create_class(&mut self, name: String) -> ClassId {
        let id = ClassId(self.classes.len() as _);
        self.classes.push(Class {
            id,
            name,
            methods: Default::default(),
        });
        id
    }

    fn create_object(&mut self, class: ClassId) -> ObjectId {
        let id = ObjectId(self.objects.len() as _);
        self.objects.push(Object {
            id,
            class,
            fields: Default::default(),
        });
        id
    }


    pub fn init(&mut self) {
        self.create_class("Nil".into());
        self.create_class("String".into());
    }
}
