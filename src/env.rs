use std::collections::HashMap;

use crate::obj::Object;

pub struct Environment {
    store : HashMap<String,Object>
}

impl Environment {
    pub fn get(&self,name: &String) -> Option<Object> {
        match self.store.get(name) {
            Some(obj) => Some(obj.clone()),
            None => None
        }
    }

    pub fn set(&mut self,name: String,val:Object){
        self.store.insert(name, val);
    }

    pub fn new() -> Self {
        Environment { store: HashMap::new() }
    }

}