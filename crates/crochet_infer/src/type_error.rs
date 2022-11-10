#[derive(Debug, PartialEq, Eq)]
pub struct TypeError {
    pub msg: String,
}

impl From<String> for TypeError {
    fn from(msg: String) -> Self {
        TypeError { msg }
    }
}

impl From<&str> for TypeError {
    fn from(msg: &str) -> Self {
        TypeError {
            msg: msg.to_owned(),
        }
    }
}
