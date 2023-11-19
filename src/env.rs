use std::{collections::HashMap, rc::Rc, cell::RefCell};

use crate::obj::Object;

#[derive(Debug,Clone)]
pub struct Environment {
    store : HashMap<String,Object>,
    outer: Option<Rc<RefCell<Environment>>>
}

impl Environment {
    pub fn get(&self,name: &String) -> Option<Object> {
        match self.store.get(name) {
            Some(obj) => Some(obj.clone()),
            None => {
                match &self.outer {
                    Some(out) => out.borrow().get(name),
                    None => None
                }
            }
        }
    }

    pub fn set(&mut self,name: String,val:Object){
        self.store.insert(name, val);
    }

    pub fn new() -> Self {
        Environment { store: HashMap::new() ,outer: None }
    }

    pub fn new_enclosed_env(outer: Rc<RefCell<Environment>>) -> Environment {
        let mut env = Environment::new();
        env.outer = Some(outer);
        env
    }

}