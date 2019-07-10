extern crate lang_c;
extern crate clap;
use lang_c::driver::{Config, parse, Error}; 
use lang_c::{ast, span, visit};
use lang_c::visit::Visit;
use clap::{Arg, App};
use std::fmt;

#[derive(Debug, Clone)]
struct FFFtype {
    return_type: Option<String>,
    pub is_vararg: bool,    
}

#[derive(Debug, Clone)]
struct FFFargument {
    name: String,
    type_ids: Vec<String>,
    typedef: String,
}

#[derive(Debug, Clone)]
struct FFFargumentList {
    arguments: Vec<FFFargument>,
}

#[derive(Debug, Clone)]
struct FFFFdecl {
    function_type: FFFtype,
    name: String,
    arguments: FFFargumentList,
} 

#[derive(Debug, Clone)]
struct Cproto2fff {
    debug: bool,
    number_of_decls: usize,
    number_of_prototypes: usize,
    declarations: Vec<FFFFdecl>,
}

impl<'ast> visit::Visit<'ast> for Cproto2fff {
     fn visit_declaration(&mut self, expr: &'ast ast::Declaration, span: &'ast span::Span) {
        self.number_of_decls += 1;
        'decl_loop:
        for decl1 in &expr.declarators {
            for spec in &expr.specifiers {
                if let ast::DeclarationSpecifier::StorageClass(sclass) = &spec.node {
                    if let ast::StorageClassSpecifier::Typedef = &sclass.node {
                        break 'decl_loop;
                    }
                }
            }
            for decl2 in &decl1.node.declarator.node.derived {                
                if let ast::DerivedDeclarator::Function(_f_expr) = &decl2.node {
                    self.number_of_prototypes +=1;
                    if self.debug {
                        println!("{:?}", expr);
                    }                    
                    self.add_declaration(expr);
                }
            }               
        }                
        visit::visit_declaration(self, expr, span);
     }
}

impl FFFtype {
    pub fn from_declaration(expr: &ast::Declaration) -> FFFtype
    {
        let mut ret_type_str: String = String::new();

        fn add_specs(specs: &Vec<span::Node<ast::DeclarationSpecifier>>) -> String 
        {
            let mut result: String = String::new();        

            if let Some((last, rest)) = specs.split_last() {
                for s in rest {
                    let spec_string = s.node.to_string();
                    if spec_string.len() > 0 {
                        result.push_str(&spec_string);
                        result.push(' ');
                    }                                        
                }
                result.push_str(&last.node.to_string());
            }
            result
        }

        ret_type_str.push_str(&add_specs(&expr.specifiers));
        for d in &expr.declarators {
            if let ast::DeclaratorKind::Identifier(_) = d.node.declarator.node.kind.node {                
                for deriv in &d.node.declarator.node.derived {
                    if let ast::DerivedDeclarator::Pointer(_) = deriv.node {
                        ret_type_str.push_str(" *")
                    }
                }            
            }
        }        
        if ret_type_str == "void" {
            FFFtype { return_type: None, is_vararg: false }
        } else {
            FFFtype { return_type: Some(ret_type_str), is_vararg: false }            
        }
    }
}

enum FFFForm {
    NormalArg(String),
    FunctionArg(ast::FunctionDeclarator),
    ArgWithTypedef((String, String))
}

trait FFFEncode {
    fn to_fff_form(&self) -> FFFForm;
}

trait ToString {
    fn to_string(&self) -> String;
}

trait ToFFFArgument {
    fn to_fff_arg(&self, function_name: Option<&String>) -> FFFargument;
}

impl ToString for ast::PointerQualifier
{
    fn to_string(&self) -> String
    {
        match self {
            ast::PointerQualifier::TypeQualifier(type_qual) => type_qual.node.to_string(),
            _ => String::new(),
        }
    }
}

impl FFFEncode for ast::DerivedDeclarator
{
    fn to_fff_form(&self) -> FFFForm
    {                
        match self {
            ast::DerivedDeclarator::Pointer(ptr) => {
                let mut result = String::new();
                for ptr_qual in ptr {
                    result.push_str(&ptr_qual.node.to_string());
                }                
                result.push_str("*");
                FFFForm::NormalArg(result)
            }
            ast::DerivedDeclarator::Function(f) => FFFForm::FunctionArg(f.node.clone()),
            _  => FFFForm::NormalArg(String::new()),
        }
    }
}


impl ToString for ast::ParameterDeclaration
{
    fn to_string(&self) -> String
    {
        let mut result = String::new();
        for spec in &self.specifiers {
            let spec_str = spec.node.to_string();
            if spec_str.len() > 0 {
                result.push_str(&spec_str);
                result.push(' ');
            }            
        }

/*
         if let Some(decl) = &self.declarator {
            result.push_str(&decl.node.to_string());
        }   
        */
        result
    }
}

impl ToFFFArgument for ast::ParameterDeclaration
{
    fn to_fff_arg(&self, function_name: Option<&String>) -> FFFargument
    {
        let mut type_ids: Vec<String> = self.specifiers.iter().map(|x| x.node.to_string()).collect();
        let mut name = String::new();
        let mut typedef = String::new();
        if let Some(decl) = &self.declarator {
            match &decl.node.kind.node {
                ast::DeclaratorKind::Identifier(n) => name = n.node.name.clone(),
                _ => {},
            }

            let array: Vec<&ast::ArrayDeclarator> = decl.node.derived.iter().filter_map(|x| match &x.node {
                ast::DerivedDeclarator::Array(a) => Some(&a.node),                
                _ => None,
            }).collect();

            for _ in 1..array.len() {
                type_ids.push("*".to_string());
            }

            let fct: Vec<&ast::FunctionDeclarator> = decl.node.derived.iter().filter_map(|x| match &x.node {
                ast::DerivedDeclarator::Function(f) => Some(&f.node),                
                _ => None,
            }).collect();

            if fct.len() > 0 {
                for parameter in &fct[0].parameters {
                    let decode_param = parameter.node.to_fff_arg(None);
                    let mut all_type_ids = type_ids.iter().map(|x| fmt::format(format_args!("{} ", x))).collect::<String>();
                    let func_param_name = 
                        if let ast::DeclaratorKind::Declarator(decl2) = &decl.node.kind.node {                     
                                for derived in &decl2.node.derived {
                                    match &derived.node {
                                        ast::DerivedDeclarator::Pointer(ptr) => {
                                                all_type_ids.push_str("*");
                                                for x in ptr {                                                
                                                    all_type_ids.push_str(&x.node.to_string());
                                                }
                                        }
                                        _ => {},                                        
                                    }
                                }
                                if let ast::DeclaratorKind::Identifier(ident) = &decl2.node.kind.node {
                                    Some(&ident.node.name)
                                }  else {
                                    None
                                }                                
                        } else {
                            None
                        };                                      
                    let fct_type_name = format!("gen_cb_{}_{}",
                                    function_name.unwrap_or(&"".to_string()),
                                    func_param_name.unwrap_or(&"".to_string()));
                    typedef = format!("typedef {}{}({})", all_type_ids, fct_type_name, decode_param);
                    type_ids = vec![fct_type_name];
                }
            } else {
                for derived in &decl.node.derived {
                    let mut pointer_quals = vec![String::new()];
                    match &derived.node {
                        ast::DerivedDeclarator::Pointer(ptr) => {
                            pointer_quals.push("*".to_string());
                            for x in ptr {
                                pointer_quals.push(x.node.to_string());
                            }
                        }
                         _ => {},
                    }                                    
                    type_ids.append(&mut pointer_quals);                
                }                
            }
        }
        FFFargument { type_ids, name, typedef }
    }
}
impl ToString for ast::TypeQualifier
{
    fn to_string(&self) -> String
    {
        match self {
            ast::TypeQualifier::Const => "const".to_string(),
            ast::TypeQualifier::Restrict => "restrict".to_string(),
            ast::TypeQualifier::Volatile => "volatile".to_string(),
            ast::TypeQualifier::Atomic => "atomic".to_string(),
           _ => "".to_string(),
        }
    }
}

impl ToString for ast::EnumType {

    fn to_string(&self) -> String {
        if let Some(identifier) = &self.identifier {
            identifier.node.name.clone()
        } else {
            "inline enum definition not supported".to_string()
        }
    }
}

impl ToString for ast::StructType {

    fn to_string(&self) -> String {
        let mut result = String::new();

        match self.kind.node {
            ast::StructKind::Struct => result.push_str("struct "),
            ast::StructKind::Union => result.push_str("union "),
        }

        if let Some(ident) = &self.identifier {
            result.push_str(&ident.node.name);
        }
        
        result
    }
}

impl ToString for ast::TypeSpecifier
{
    fn to_string(&self) -> String
    {
        match self {
            ast::TypeSpecifier::Atomic(_) => "atomic".to_string(),
            ast::TypeSpecifier::Bool => "bool".to_string(),
            ast::TypeSpecifier::Char => "char".to_string(),
            ast::TypeSpecifier::Complex => "complex".to_string(),
            ast::TypeSpecifier::Double => "double".to_string(),
            ast::TypeSpecifier::Enum(c_enum) => c_enum.node.to_string(),
            ast::TypeSpecifier::Float => "float".to_string(),
            ast::TypeSpecifier::Int => "int".to_string(),
            ast::TypeSpecifier::Long => "long".to_string(),
            ast::TypeSpecifier::Short => "short".to_string(),
            ast::TypeSpecifier::Signed => "signed".to_string(),
            ast::TypeSpecifier::Struct(c_struct) => c_struct.node.to_string(),
            ast::TypeSpecifier::TS18661Float(_) => "unsupported TS18661Float".to_string(),
            ast::TypeSpecifier::TypeOf(_) => "unsupported typeof".to_string(),
            ast::TypeSpecifier::TypedefName(typedef_name) => typedef_name.node.name.clone(),
            ast::TypeSpecifier::Unsigned => "unsigned".to_string(),
            ast::TypeSpecifier::Void => "void".to_string(),
        }   
    }   
}

impl ToString for ast::DeclarationSpecifier
{
    fn to_string(&self) -> String
    {
        match self {
            ast::DeclarationSpecifier::TypeQualifier(qual) => qual.node.to_string(),
            ast::DeclarationSpecifier::TypeSpecifier(specifier) => specifier.node.to_string(),
            _ => String::new(),
        }
    }
}

impl Cproto2fff {
    pub fn add_declaration(&mut self, expr: &ast::Declaration)
    {
        let mut fff_fdecl: FFFFdecl = FFFFdecl::new();
        
        fff_fdecl.function_type = FFFtype::from_declaration(expr);

        for d in &expr.declarators {
           if let ast::DeclaratorKind::Identifier(name) = &d.node.declarator.node.kind.node {
                fff_fdecl.name = name.node.name.clone();
            }    
        }

        for d in &expr.declarators {
            for deriv in &d.node.declarator.node.derived {
                if let ast::DerivedDeclarator::Function(f) = &deriv.node {
                    for param in &f.node.parameters {
                        fff_fdecl.arguments.arguments.push(param.node.to_fff_arg(Some(&fff_fdecl.name)));
                    }
                    if f.node.ellipsis == ast::Ellipsis::Some {
                        fff_fdecl.function_type.is_vararg = true;
                    }
                }   
            }   
        }


        self.declarations.push(fff_fdecl);
    }

    pub fn config(debug: bool) -> Cproto2fff
    {
        Cproto2fff { debug, number_of_decls: 0, number_of_prototypes: 0, declarations: vec![] }
    }
}

impl fmt::Display for FFFargument
{
    fn fmt(&self, dest: &mut fmt::Formatter) -> fmt::Result
    {
        if self.type_ids.len() != 1 || self.type_ids[0] != "void" {
            let (last, rest) = self.type_ids.split_last().unwrap();
            let results: String = rest.iter().map(|x| fmt::format(format_args!("{} ", x))).collect();
            write!(dest, "{}{}", results, last)
        } else {
            Ok(())
        }
    }
}

impl fmt::Display for FFFargumentList
{
    fn fmt(&self, dest: &mut fmt::Formatter) -> fmt::Result
    {
        if self.arguments.len() == 1 {
            let results: String = self.arguments.iter().map(|x| fmt::format(format_args!("{}", x))).collect();
            if results.len() > 0 {
                write!(dest, ", {}", results)
            } else {
                Ok(())
            }
        } else {
            self.arguments.iter().map(|x| write!(dest, ", {}", x)).collect()
        }
    }
}

impl fmt::Display for FFFtype {
    fn fmt(&self, dest: &mut fmt::Formatter) -> fmt::Result
    {
        let formatted_type_str =
            if let Some(type_str) = &self.return_type {
                format!("{}, ", type_str)
            } else {
                String::new()
            };                           

        write!(dest, "{}{}{}",
            match self.return_type {
                None => "FAKE_VOID_FUNC",
                Some(_) => "FAKE_VALUE_FUNC",
            },
            if self.is_vararg {
                "_VARARG("                
            } else {
                "("
            }, formatted_type_str)        
    }
}

impl fmt::Display for FFFFdecl {
    fn fmt(&self, dest: &mut fmt::Formatter) -> fmt::Result
    {
        let ellipsis = if self.function_type.is_vararg {
                           ",..."
                        } else {
                            ""
                        };
        let typedefs: String = self.arguments.arguments.iter().filter_map(|x| {
            if x.typedef.len() > 0 {
                Some(fmt::format(format_args!("{};\n", x.typedef)))
            } else {
                None
            }
        }).collect();
        
        writeln!(dest, "{}{}{}{}{})", typedefs, self.function_type, self.name, self.arguments, ellipsis)
    }
}

impl fmt::Display for Cproto2fff {

    fn fmt(&self, dest: &mut fmt::Formatter) -> fmt::Result
    {
       self.declarations.iter().map(|x| x.fmt(dest)).collect()
    }
}

impl FFFFdecl {
    pub fn new() -> FFFFdecl
    {
        FFFFdecl {
            function_type: FFFtype { return_type: None, is_vararg: false },
            name: String::new(),
            arguments: FFFargumentList::new(),
        }
    }
}

impl FFFargumentList {
    pub fn new() -> FFFargumentList
    {
        FFFargumentList { arguments: vec![]}
    }
}

fn main()
{
    let cli_matches = App::new("cproto2fff")
                        .version("1.0")
                        .author("Peter De Schrijver")
                        .about("Generate fake functions from C prototypes")
                        .arg(Arg::with_name("INPUT")
                            .help("C Input file to parse")
                            .required(true)                            
                            .index(1))
                        .arg(Arg::with_name("include")
                            .help("Include path")                            
                            .short("I")
                            .takes_value(true)
                            .multiple(true)
                            .number_of_values(1)
                            .long("include"))                            
                        .arg(Arg::with_name("debug")
                            .help("Debug")
                            .short("d")
                            .long("debug"))
                        .get_matches();
    let filename = cli_matches.value_of("INPUT").unwrap();
    let mut config = Config::with_gcc();
    if let Some(include_files) = cli_matches.values_of("include") {
        for f in include_files {
            config.cpp_options.push("-I".to_string());
            config.cpp_options.push(f.to_string());
        }
    }
    let mut cproto2fff = Cproto2fff::config(cli_matches.is_present("debug"));

    match parse(&config, filename) {
        Ok(parse_result) => cproto2fff.visit_translation_unit(&parse_result.unit),
        Err(error) => {
            match error {
                Error::PreprocessorError(preproc_err) => {                
                    println!("Preprocessor Error {}: {}", filename, preproc_err);
                }
                Error::SyntaxError(syntax_err) => {
                    println!("{} offset {}",  syntax_err, syntax_err.offset);
                }
            }
        }
    }
    if cproto2fff.debug {
        println!("Declarations found: {}", cproto2fff.number_of_decls);
        println!("Function prototype declarations found: {}", cproto2fff.number_of_prototypes);
    }        
    print!("{}", cproto2fff);
}
