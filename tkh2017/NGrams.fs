module NGrams

let moduleTok =
    [['m'], false, false;
     ['o'], false, false;
     ['d'], false, false;
     ['u'], false, false;
     ['l'], false, false;
     ['e'], false, false;
     [' '], false, false]

let identifierTok =
    [['a'..'z']@['A'..'Z'], true, false;
     ['0'..'9'], true, true;
     [' '], true, true]

let punctuationTok = 
    [['('; ')'; '['; ']'; '{'; '}'; ':'; ';'; ','], false, false]

let andTok = 
    [['a'], false, false;
     ['n'], false, false;
     ['d'], false, false;
     [' '], false, false]

let orTok = 
    [['o'], false, false;
     ['r'], false, false;
     [' '], false, false]

let notTok = 
    [['n'], false, false;
     ['o'], false, false;
     ['t'], false, false;
     [' '], false, false]

let inputTok = 
    [['i'], false, false;
     ['n'], false, false;
     ['p'], false, false;
     ['u'], false, false;
     ['t'], false, false]

let outputTok = 
    [['o'], false, false;
     ['u'], false, false;
     ['t'], false, false;
     ['p'], false, false;
     ['u'], false, false;
     ['t'], false, false]

let endModuleTok =
    [['e'], false, false;
     ['n'], false, false;
     ['d'], false, false;
     ['m'], false, false;
     ['o'], false, false;
     ['d'], false, false;
     ['u'], false, false;
     ['l'], false, false;
     ['e'], false, false]