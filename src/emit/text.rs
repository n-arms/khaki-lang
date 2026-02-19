#[derive(Default)]
pub struct Text {
    pub indent: usize,
    pub text: String,
    pub line: String,
}

impl Text {
    pub fn inc(&mut self) {
        self.indent += 1;
    }

    pub fn dec(&mut self) {
        self.indent -= 1;
    }

    pub fn push(&mut self, text: impl AsRef<str>) {
        self.line.push_str(text.as_ref());
    }

    pub fn newline(&mut self) {
        for _ in 0..self.indent {
            self.text.push_str("  ");
        }
        self.text.push_str(&self.line);
        self.text.push('\n');
        self.line.clear();
    }

    pub fn pushln(&mut self, text: impl AsRef<str>) {
        self.push(text);
        self.newline();
    }

    pub fn finish(mut self) -> String {
        if !self.line.is_empty() {
            self.newline();
        }
        self.text
    }
}

#[derive(Default)]
pub struct LlvmVals {
    next: usize,
}

impl LlvmVals {
    pub fn fresh(&mut self) -> String {
        let id = self.next;
        self.next += 1;
        format!("%var{id}")
    }
}
