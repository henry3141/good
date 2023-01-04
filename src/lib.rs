extern crate pest;
#[macro_use]
extern crate pest_derive;
use std::{collections::HashMap, ops::{Add, Sub, Mul, Rem, Div, Neg, Not, BitOr, BitAnd}, cmp::Ordering, hash::{Hash, Hasher}};

use pest::{Parser, iterators::Pairs};
#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct RlangParser;

pub fn smart_print(pairs: Pairs<Rule>,indent:usize) {
    let mut add = " ".to_string();
    for _ in 0..indent {
        add = format!("{} ",add);
    }
    for pair in pairs {
        println!("{}{:#?}: {:?}",add, pair.as_rule(), pair.as_str());
        smart_print(pair.into_inner(),indent+2);
    }
}

#[derive(Debug,Clone,PartialEq)]
pub struct Rlang {
    pub code: String,
    pub program: Option<Program>,
    pub state: Option<Intepreter>,
}

impl Rlang {
    pub fn new(s:String) -> Rlang {
        Rlang {
            code: s,
            program: None,
            state: None,
        }
    }

    pub fn parse(&mut self) {
        self.remove();
        let rules = RlangParser::parse(Rule::program, &self.code).unwrap().collect::<Vec<_>>();
        self.program = Some(Program::new(rules));

    }

    pub fn run(&mut self) {
        self.state = Some(Intepreter::new(self.program.clone().unwrap()));
        self.state.as_mut().unwrap().run();
    }
    
    ///remove \n \r and spaces from code
    fn remove(&mut self) {
        let mut in_string = false;
        let mut new_code = String::new();
        for c in self.code.chars() {
            if c == '"' {
                in_string = !in_string;
            }
            if !in_string {
                if c == ' ' || c == '\n' || c == '\r' {
                    continue;
                } else {
                    new_code.push(c);
                }
            } else {
                new_code.push(c);
            }
    	}
        self.code = new_code;
    }
}


#[derive(Debug,Clone,PartialEq)]
pub struct Program {
    statements: Vec<Statement >,
}

#[derive(Debug,Clone,PartialEq)]
enum Statement {
    Math(Math),
    FuncDef(FuncDef),
    FuncCall(FuncCall),
    Assign(Assign),
    If(If),
    For(For),
    While(While),
    Var(String),
    Const(Const),
    Print(Print),
    Select(Select),
}

#[derive(Debug,Clone,PartialEq)]
struct Print {
    val: Box<Statement>,
}

#[derive(Debug,Clone,PartialEq)]
struct Math {
    lhs: Box< Statement >,
    op: Op,
    rhs: Box< Statement >,
}

#[derive(Debug,Clone,PartialEq)]
struct FuncDef {
    name: String,
    args: Vec<String>,
    body: Box<Statement>,
}

#[derive(Debug,Clone,PartialEq)]
struct FuncCall {
    name: String,
    args: Vec<Statement>,
}

#[derive(Debug,Clone,PartialEq)]
struct If {
    cond: Box<Statement>,
    then_branch: Box<Statement>,
    else_branch: Option<Box<Statement>>,
}

#[derive(Debug,Clone,PartialEq)]
struct For {
    var: String,
    range: Box<Statement>,
    body: Box<Statement>,
}

#[derive(Debug,Clone,PartialEq)]
struct While {
    cond: Box< Statement >,
    body: Box< Statement >,
}

#[derive(Debug,Clone,PartialEq)]
enum Const {
    String(String),
    Number(f64),
    Array(Array),
    Object(Object),
    Select(Select),
}

#[derive(Debug,Clone,PartialEq)]
struct Assign {
    var: String,
    val: Box<Statement>,
}

#[derive(Debug,Clone,PartialEq)]
struct Array {
    vals: Vec<Statement>,
}

#[derive(Debug,Clone,PartialEq)]
struct Object {
    vals: Vec<(Statement,Statement)>,
}

#[derive(Debug,Clone,PartialEq)]
struct Select {
    obj: Box<Statement>,
    key: Box<Statement>,
}


#[derive(Debug,Clone,PartialEq)]
struct Object_object {
    key: Statement,
    val: Statement,
}

#[derive(Debug,Clone,PartialEq)]
enum Op {
    Add,
    Sub,
    Mult,
    Div,
    Pow,
    Mod,
    Equal,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    NotEqual,
    And,
    Or,
}


impl Program {
    fn new(mut rules: Vec< pest::iterators::Pair<Rule> >) -> Program {
        let mut statements = Vec::new();
        rules = (&rules[0]).clone().into_inner().collect::<Vec<_>>();
        for rule in rules {
            statements.push(Statement::new(rule));
        }
        Program {
            statements,
        }
    }

    pub fn run(&self,state:&mut Intepreter) {
        for statement in &self.statements {
            statement.run(state);
        }
    }
}

impl Statement {
    fn new(rule: pest::iterators::Pair<Rule>) -> Statement {
        match rule.as_rule() {
            Rule::math => Statement::Math(Math::new(rule)),
            Rule::func_def => Statement::FuncDef(FuncDef::new(rule)),
            Rule::func_call => Statement::FuncCall(FuncCall::new(rule)),
            Rule::assign => Statement::Assign(Assign::new(rule)),
            Rule::if_statement => Statement::If(If::new(rule)),
            Rule::for_statement => Statement::For(For::new(rule)),
            Rule::while_statement => Statement::While(While::new(rule)),
            Rule::var => Statement::Var(rule.as_str().to_string()),
            Rule::const_statement => Statement::Const(Const::new(rule)),
            Rule::statement => Statement::new(rule.into_inner().next().unwrap()),
            Rule::string => Statement::Const(Const::String(rule.as_str().to_string())),
            Rule::number => Statement::Const(Const::Number(rule.as_str().parse::<f64>().unwrap())),
            Rule::array => Statement::Const(Const::Array(Array::new(rule))),
            Rule::print => Statement::Print(Print::new(rule)),
            Rule::object => Statement::Const(Const::Object(Object::new(rule))),
            Rule::object_object => Statement::Const(Const::Object(Object::new(rule))),
            Rule::select => Statement::Select(Select::new(rule)),
            _ => panic!("Unknown rule: {:?}",rule.as_rule()),
        }
    }

    fn run(&self,state:&mut Intepreter) -> Data {
        match self {
            Statement::Math(math) => math.run(state),
            Statement::FuncDef(func_def) => func_def.run(state),
            Statement::FuncCall(func_call) => func_call.run(state),
            Statement::Assign(assign) => assign.run(state),
            Statement::If(if_statement) => if_statement.run(state),
            Statement::For(for_statement) => for_statement.run(state),
            Statement::While(while_statement) => while_statement.run(state),
            Statement::Var(var) => state.get_var(var),
            Statement::Const(const_statement) => const_statement.run(state),
            Statement::Print(print) => print.run(state),
            Statement::Select(select) => select.run(state),
        }
    }
}

impl Object_object {
    fn new(rule: pest::iterators::Pair<Rule>) -> Object_object {
        let mut rules = rule.into_inner();
        let key = Statement::new(rules.next().unwrap());
        let val = Statement::new(rules.next().unwrap());
        Object_object {
            key,
            val,
        }
    }

    fn run(&self,state:&mut Intepreter) -> Data {
        let key = self.key.run(state);
        let val = self.val.run(state);
        Data::KeyPair(Box::new(key),Box::new(val))
    }
}

impl Print {
    fn new(rule: pest::iterators::Pair<Rule>) -> Print {
        let mut rules = rule.into_inner();
        let val = Box::new(Statement::new(rules.next().unwrap()));
        Print {
            val,
        }
    }

    fn run(&self,state:&mut Intepreter) -> Data {
        let val = self.val.run(state);
        println!("{}",val.to_string());
        Data::Null
    }
}


impl Math {
    fn new(rule: pest::iterators::Pair<Rule>) -> Math {
        let mut rules = rule.into_inner();
        let lhs = Box::new(Statement::new(rules.next().unwrap()));
        let op = Op::new(rules.next().unwrap());
        let rhs = Box::new(Statement::new(rules.next().unwrap()));
        Math {
            lhs,
            op,
            rhs,
        }
    }

    fn run(&self,state:&mut Intepreter) -> Data {
        let lhs = self.lhs.run(state);
        let rhs = self.rhs.run(state);
        match self.op {
            Op::Add => lhs + rhs,
            Op::Sub => lhs - rhs,
            Op::Mult => lhs * rhs,
            Op::Div => lhs / rhs,
            Op::Pow => todo!("pow"),
            Op::Mod => lhs % rhs,
            Op::Equal => {
                if lhs == rhs {
                    Data::Number(1.0)
                } else {
                    Data::Number(0.0)
                }
            },
            Op::GreaterThan => 
                if lhs > rhs {
                    Data::Number(1.0)
                } else {
                    Data::Number(0.0)
                },
            Op::LessThan => 
                if lhs < rhs {
                    Data::Number(1.0)
                } else {
                    Data::Number(0.0)
                },
            Op::GreaterThanOrEqual => {
                if lhs >= rhs {
                    Data::Number(1.0)
                } else {
                    Data::Number(0.0)
                }},
            Op::LessThanOrEqual => {
                if lhs <= rhs {
                    Data::Number(1.0)
                } else {
                    Data::Number(0.0)
                }},
            Op::NotEqual => {
                if lhs != rhs {
                    Data::Number(1.0)
                } else {
                    Data::Number(0.0)
                }},
            _ => todo!("op"),
        }
    }
}

impl FuncDef {
    fn new(rule: pest::iterators::Pair<Rule>) -> FuncDef {
        let mut rules = rule.into_inner();
        let name = rules.next().unwrap().as_str().to_string();
        let mut args = Vec::new();
        let mut arg = rules.next();
        let mut body:Option<Box<Statement>> = None;
        while arg.is_some() {
            args.push(arg.unwrap().as_str().to_string());
            arg = rules.next();
            if arg.is_some() {
                if (&arg).clone().unwrap().as_rule() == Rule::statement {
                    body = Some(Box::new(Statement::new(arg.unwrap())));
                    break;
                }
            }
        }
        FuncDef {
            name,
            args,
            body:body.unwrap(),
        }
    }

    fn run(&self,state:&mut Intepreter) -> Data {
        state.add_func(self.name.clone(),self.args.clone(),*self.body.clone());
        Data::Null
    }
}

impl FuncCall {
    fn new(rule: pest::iterators::Pair<Rule>) -> FuncCall {
        let mut rules = rule.into_inner();
        let name = rules.next().unwrap().as_str().to_string();
        let mut args = Vec::new();
        let mut arg = rules.next();
        while arg.is_some() {
            args.push(Statement::new(arg.unwrap()));
            arg = rules.next();
        }
        FuncCall {
            name,
            args,
        }
    }
    fn run(&self,state:&mut Intepreter) -> Data {
        let mut args = Vec::new();
        for arg in &self.args {
            args.push(arg.run(state));
        }
        state.call_func(&self.name,args)
    }
}

impl If {
    fn new(rule: pest::iterators::Pair<Rule>) -> If {
        let mut rules = rule.into_inner();
        let cond = Box::new(Statement::new(rules.next().unwrap()));
        let then_branch = Box::new(Statement::new(rules.next().unwrap()));
        let else_branch = match rules.next() {
            Some(rule) => Some(Box::new(Statement::new(rule))),
            None => None,
        };
        If {
            cond,
            then_branch,
            else_branch,
        }
    }

    fn run(&self,state:&mut Intepreter) -> Data {
        if self.cond.run(state).to_bool() {
            self.then_branch.run(state)
        } else {
            match &self.else_branch {
                Some(else_branch) => else_branch.run(state),
                None => Data::Null,
            }
        }
    }


}

//index
impl Select {
    fn new(rule: pest::iterators::Pair<Rule>) -> Select {
        let mut rules = rule.into_inner();
        let lhs = Box::new(Statement::new(rules.next().unwrap()));
        let rhs = Box::new(Statement::new(rules.next().unwrap()));
        Select {
            obj:lhs,
            key:rhs,
        }
    }

    fn run(&self,state:&mut Intepreter) -> Data {
        let obj = self.obj.run(state);
        let key = self.key.run(state);
        match obj {
            Data::Array(array) => {
                match key {
                    Data::Number(num) => {
                        let index = num as usize;
                        if index < array.len() {
                            array[index].clone()
                        } else {
                            Data::Null
                        }
                    },
                    _ => todo!("select"),
                }
            },
            Data::Object(obj) => {
                obj.get(&key).unwrap_or(&Data::Null).clone()
            }
            _ => todo!("select"),
        }
    }

}

impl For {
    fn new(rule: pest::iterators::Pair<Rule>) -> For {
        let mut rules = rule.into_inner();
        let var = rules.next().unwrap().as_str().to_string();
        let range = Box::new(Statement::new(rules.next().unwrap()));
        let body = Box::new(Statement::new(rules.next().unwrap()));
        For {
            var,
            range,
            body,
        }
    }

    fn run(&self,state:&mut Intepreter) -> Data {
        //always return None
        let range = self.range.run(state);
        match range {
            Data::Array(array) => {
                for item in array {
                    state.set_var(&self.var,item);
                    self.body.run(state);
                }
            },
            Data::Number(number) => {
                for i in 0..number as i64 {
                    state.set_var(&self.var,Data::Number(i as f64));
                    self.body.run(state);
                }
            },
            _ => panic!("For loop range must be a number or an array"),
        };
        Data::Null
    }
}

impl While {
    fn new(rule: pest::iterators::Pair<Rule>) -> While {
        let mut rules = rule.into_inner();
        let cond = Box::new(Statement::new(rules.next().unwrap()));
        let body = Box::new(Statement::new(rules.next().unwrap()));
        While {
            cond,
            body,
        }
    }

    fn run(&self,state:&mut Intepreter) -> Data {
        while self.cond.run(state).to_bool() {
            self.body.run(state);
        }
        Data::Null
    }
}

impl Const {
    fn new(rule: pest::iterators::Pair<Rule>) -> Const {
        match rule.as_rule() {
            Rule::string => Const::String({let mut x = rule.as_str().to_string();x.remove(0);x.pop();x}),
            Rule::number => Const::Number(rule.as_str().parse::<f64>().unwrap()),
            Rule::array => Const::Array(Array::new(rule)),
            Rule::object => Const::Object(Object::new(rule)),
            Rule::const_statement => Const::new(rule.into_inner().next().unwrap()),
            Rule::select => Const::Select(Select::new(rule)),
            _ => panic!("Unknown rule: {:?}",rule.as_rule()),
        }
    }

    fn run(&self,state:&mut Intepreter) -> Data {
        match self {
            Const::String(string) => Data::String(string.clone()),
            Const::Number(number) => Data::Number(*number),
            Const::Array(array) => array.run(state),
            Const::Object(object) => object.run(state),
            Const::Select(select) => select.run(state),
        }
    }
}

impl Assign {
    fn new(rule: pest::iterators::Pair<Rule>) -> Assign {
        let mut rules = rule.into_inner();
        let var = rules.next().unwrap().as_str().to_string();
        let val = Box::new(Statement::new(rules.next().unwrap()));
        Assign {
            var,
            val,
        }
    }

    fn run(&self,state:&mut Intepreter) -> Data {
        let val = self.val.run(state);
        state.set_var(&self.var,val.clone());
        Data::Null
    }
}

impl Op {
    fn new(rule: pest::iterators::Pair<Rule>) -> Op {
        //match inner string
        match rule.as_str() {
            "+" => Op::Add,
            "-" => Op::Sub,
            "*" => Op::Mult,
            "/" => Op::Div,
            "%" => Op::Mod,
            "^" => Op::Pow,
            "==" => Op::Equal,
            ">" => Op::GreaterThan,
            "<" => Op::LessThan,
            ">=" => Op::GreaterThanOrEqual,
            "<=" => Op::LessThanOrEqual,
            "!=" => Op::NotEqual,
            "&&" => Op::And,
            "||" => Op::Or,
            _ => panic!("Unknown op: {:?}",rule.as_str()),
        }
    }

    fn run(&mut self,state:&mut Intepreter) -> Data {
        panic!("Op::run() should never be called");
    }
}

impl Array {
    fn new(rule: pest::iterators::Pair<Rule>) -> Array {
        let mut values = Vec::new();
        for value in rule.into_inner() {
            values.push(Statement::new(value));
        }
        Array {
            vals: values,
        }
    }

    fn run(&self,state:&mut Intepreter) -> Data {
        let mut values = Vec::new();
        for value in &self.vals {
            values.push(value.run(state));
        }
        Data::Array(values)
    }
}

impl Object {
    fn new(rule: pest::iterators::Pair<Rule>) -> Object {
        let mut values:Vec<(Statement,Statement)> = vec![];
        let inner = rule.into_inner();
        for pair in inner {
            let mut inner = pair.into_inner();
            values.push((Statement::new(inner.next().unwrap()),Statement::new(inner.next().unwrap())));
        }
        Object {
            vals: values,
        }
    }

    fn run(&self,state:&mut Intepreter) -> Data {
        let mut values = HashMap::new();
        for (key,val) in &self.vals {
            values.insert(key.run(state),val.run(state));
        }
        Data::Object(values)
    }
}

#[derive(Debug,Clone)]
enum Data {
    String(String),
    Number(f64),
    Array(Vec<Data>),
    Object(HashMap<Data,Data>),
    Func(FuncDef),
    KeyPair(Box<Data>,Box<Data>),
    Null,
}

impl Data {
    fn to_bool(&self) -> bool {
        match self {
            Data::Number(number) => *number != 0.0,
            _ => panic!("Cannot convert {:?} to bool",self),
        }
    }

    fn to_string(&self) -> String {
        match self {
            Data::String(string) => string.clone(),
            Data::Number(number) => number.to_string(),
            Data::Array(array) => {
                let mut string = String::new();
                for (i,val) in array.iter().enumerate() {
                    if i != 0 {
                        string.push_str(", ");
                    }
                    string.push_str(&val.to_string());
                }
                string
            },
            Data::Object(object) => {
                let mut string = String::new();
                for (i,(key,val)) in object.iter().enumerate() {
                    if i != 0 {
                        string.push_str(", ");
                    }
                    string.push_str(&key.to_string());
                    string.push_str(": ");
                    string.push_str(&val.to_string());
                }
                string
            },
            Data::Null => "null".to_string(),
            _ => panic!("Cannot convert {:?} to string",self),
        }
    }
}

impl PartialEq for Data {
    fn eq(&self,other:&Data) -> bool {
        match (self,other) {
            (Data::String(a),Data::String(b)) => a == b,
            (Data::Number(a),Data::Number(b)) => a == b,
            (Data::Array(a),Data::Array(b)) => a == b,
            (Data::Object(a),Data::Object(b)) => a == b,
            (Data::Func(a),Data::Func(b)) => {
                a.name == b.name &&
                a.args == b.args &&
                a.body == b.body
            },
            (Data::Null,Data::Null) => true,
            _ => false,
        }
    }
}

impl Add for Data {
    type Output = Data;
    fn add(self,other:Data) -> Data {
        let safe_copy = (self.clone(),other.clone());
        match (self,other) {
            (Data::String(a),Data::String(b)) => Data::String(a + &b),
            (Data::Number(a),Data::Number(b)) => Data::Number(a + b),
            (Data::Array(mut a),Data::Array(b)) => {
                a.extend(b);
                Data::Array(a)
            },
            (Data::Object(mut a),Data::Object(b)) => {
                a.extend(b);
                Data::Object(a)
            },
            (Data::Object(mut a),Data::KeyPair(key,val)) => {
                a.insert(*key,*val);
                Data::Object(a)
            },
            (Data::Object(a),Data::String(b)) => {
                let mut string = String::new();
                for (i,(key,val)) in a.iter().enumerate() {
                    if i != 0 {
                        string.push_str(", ");
                    }
                    string.push_str(&key.to_string());
                    string.push_str(": ");
                    string.push_str(&val.to_string());
                }
                string.push_str(&b);
                Data::String(string)
            },
            _ => panic!("Cannot add {:?} and {:?}",safe_copy.0,safe_copy.1),
        }
    }
}

impl Sub for Data {
    type Output = Data;
    fn sub(self,other:Data) -> Data {
        let safe_copy = (self.clone(),other.clone());
        match (self,other) {
            (Data::Number(a),Data::Number(b)) => Data::Number(a - b),
            _ => panic!("Cannot subtract {:?} and {:?}",safe_copy.0,safe_copy.1),
        }
    }
}

impl Mul for Data {
    type Output = Data;
    fn mul(self,other:Data) -> Data {
        let safe_copy = (self.clone(),other.clone());
        match (self,other) {
            (Data::Number(a),Data::Number(b)) => Data::Number(a * b),
            (Data::String(a),Data::Number(b)) => {
                let mut s = String::new();
                for _ in 0..b as usize {
                    s += &a;
                }
                Data::String(s)
            },
            _ => panic!("Cannot multiply {:?} and {:?}",safe_copy.0,safe_copy.1),
        }
    }
}

impl Div for Data {
    type Output = Data;
    fn div(self,other:Data) -> Data {
        let safe_copy = (self.clone(),other.clone());
        match (self,other) {
            (Data::Number(a),Data::Number(b)) => Data::Number(a / b),
            _ => panic!("Cannot divide {:?} and {:?}",safe_copy.0,safe_copy.1),
        }
    }
}

impl Rem for Data {
    type Output = Data;
    fn rem(self,other:Data) -> Data {
        let safe_copy = (self.clone(),other.clone());
        match (self,other) {
            (Data::Number(a),Data::Number(b)) => Data::Number(a % b),
            _ => panic!("Cannot modulo {:?} and {:?}",safe_copy.0,safe_copy.1),
        }
    }
}

impl Neg for Data {
    type Output = Data;
    fn neg(self) -> Data {
        match self {
            Data::Number(a) => Data::Number(-a),
            _ => panic!("Cannot negate {:?}",self),
        }
    }
}

impl Not for Data {
    type Output = Data;
    fn not(self) -> Data {
        match self {
            Data::Number(a) => Data::Number(if a == 0.0 { 1.0 } else { 0.0 }),
            _ => panic!("Cannot negate {:?}",self),
        }
    }
}

impl PartialOrd for Data {
    fn partial_cmp(&self,other:&Data) -> Option<Ordering> {
        match (self,other) {
            (Data::Number(a),Data::Number(b)) => a.partial_cmp(b),
            _ => panic!("Cannot compare {:?} and {:?}",self,other),
        }
    }

    fn lt(&self, other: &Self) -> bool {
        matches!(self.partial_cmp(other), Some(std::cmp::Ordering::Less))
    }

    fn le(&self, other: &Self) -> bool {
        matches!(self.partial_cmp(other), Some(std::cmp::Ordering::Less | std::cmp::Ordering::Equal))
    }

    fn gt(&self, other: &Self) -> bool {
        matches!(self.partial_cmp(other), Some(std::cmp::Ordering::Greater))
    }

    fn ge(&self, other: &Self) -> bool {
        matches!(self.partial_cmp(other), Some(std::cmp::Ordering::Greater |std::cmp::Ordering::Equal))
    }
}

//bitwise operators
impl BitAnd for Data {
    type Output = Data;
    fn bitand(self,other:Data) -> Data {
        let safe_copy = (self.clone(),other.clone());
        match (self,other) {
            (Data::Number(a),Data::Number(b)) => Data::Number((a as i64 & b as i64) as f64),
            _ => panic!("Cannot bitwise and {:?} and {:?}",safe_copy.0,safe_copy.1),
        }
    }
}

impl BitOr for Data {
    type Output = Data;
    fn bitor(self,other:Data) -> Data {
        let safe_copy = (self.clone(),other.clone());
        match (self,other) {
            (Data::Number(a),Data::Number(b)) => Data::Number((a as i64 | b as i64) as f64),
            _ => panic!("Cannot bitwise or {:?} and {:?}",safe_copy.0,safe_copy.1),
        }
    }
}

impl Hash for Data {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Data::Number(a) => a.to_bits().hash(state),
            Data::String(a) => a.hash(state),
            Data::Array(a) => a.hash(state),
            Data::Null => 0.hash(state),
            _ => panic!("Cannot hash {:?}",self),
        }
    }
}

impl Eq for Data {

}







#[derive(Debug,Clone,PartialEq)]
pub struct Intepreter {
    objects: HashMap<String,Data>,
    program:Program,
}

impl Intepreter {
    pub fn new(program:Program) -> Intepreter {
        Intepreter {
            objects: HashMap::new(),
            program,
        }
    }
    pub fn run(&mut self) {
        //structure: Recursively call run on each statement;wich will call run on each substatement; the statemen.run() will return a Data object and as input will get &mut Interpreter und &mut Statement
        let program = self.program.clone();
        program.run(self);
    }
    fn add_func(&mut self,name:String,args:Vec<String>,body:Statement) {
        self.objects.insert((&name).clone(),Data::Func(FuncDef {
            name,
            args,
            body:Box::new(body),
        }));
    }
    fn call_func(&mut self,name:&str,args:Vec<Data>) -> Data {
        let func = self.objects.get(name).unwrap();
        match func {
            Data::Func(func) => {
                let mut new_state = Intepreter::new(Program {
                    statements: vec![],
                });
                for (i,arg) in args.iter().enumerate() {
                    new_state.objects.insert(func.args[i].clone(),arg.clone());
                }
                func.body.run(&mut new_state);
                new_state.objects.get("return").unwrap_or(&Data::Null).clone()
            },
            _ => panic!("Not a function"),
        }
    }

    fn get_var(&self,name:&str) -> Data {
        self.objects.get(name).unwrap().clone()
    }

    fn set_var(&mut self,name:&str,val:Data) {
        self.objects.insert(name.to_string(),val);
    }
}
