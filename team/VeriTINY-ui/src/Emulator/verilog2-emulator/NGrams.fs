module NGrams

let moduleTok =
    [['m'], false, false;
     ['o'], false, false;
     ['d'], false, false;
     ['u'], false, false;
     ['l'], false, false;
     ['e'], false, false;
     [' '], true, false]

let identifierTok =
    [['a'..'z'] @ ['A'..'Z'] @ ['_'], true, false;
     ['0'..'9'] @ ['_'] @ ['$'] @ ['a'..'z'] @ ['A'..'Z'], true, true;
     [' '], true, true]

let numberTok = 
    [['0'..'9'], true, false]

let punctuationTok = 
    [['('; ')'; '['; ']'; '{'; '}'; ':'; ';'; ','], false, false;
     [' '], true, true]

let operatorTok = 
    [['&'; '|'; '~'; '^'], false, false;
     [' '], true, true]

let assignTok = 
    [['a'], false, false;
     ['s'], false, false;
     ['s'], false, false; 
     ['i'], false, false;
     ['g'], false, false;
     ['n'], false, false;
     [' '], true, true]

let equalTok = 
    [['='], false, false; 
     [' '], true, true]

let andTok = 
    [['a'], false, false;
     ['n'], false, false;
     ['d'], false, false;
     [' '], true, false]

let orTok = 
    [['o'], false, false;
     ['r'], false, false;
     [' '], true, false]

let xorTok = 
    [['x'], false, false;
     ['o'], false, false;
     ['r'], false, false;
     [' '], true, false]

let notTok = 
    [['n'], false, false;
     ['o'], false, false;
     ['t'], false, false;
     [' '], true, false]

let inputTok = 
    [['i'], false, false;
     ['n'], false, false;
     ['p'], false, false;
     ['u'], false, false;
     ['t'], false, false;
     [' '], true, false]

let outputTok = 
    [['o'], false, false;
     ['u'], false, false;
     ['t'], false, false;
     ['p'], false, false;
     ['u'], false, false;
     ['t'], false, false;
     [' '], true, false]

let wireTok = 
    [['w'], false, false;
     ['i'], false, false;
     ['r'], false, false;
     ['e'], false, false;
     [' '], true, false]

let endModuleTok =
    [['e'], false, false;
     ['n'], false, false;
     ['d'], false, false;
     ['m'], false, false;
     ['o'], false, false;
     ['d'], false, false;
     ['u'], false, false;
     ['l'], false, false;
     ['e'], false, false] //' ' not needed due to Lexer implementation

let emptyLine = 
    [['\013'], false, true;
     ['\n'], false, false;
     [' '], true, true]