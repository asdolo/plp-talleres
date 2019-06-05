// Ejercicio 1
let qf = {
    esFinal: true,
    transiciones: {},
    acepta : (s) => s === ""
}

let q3 = {
    esFinal: true,
    transiciones: {}
}

let q2 = {
    esFinal: true,
    transiciones: {c: q3}
}

let q1 = {
    esFinal: false,
    transiciones: {b: q2, c: q3 },
}

q1.transiciones.a = q1;

console.log(q2.transiciones.c == q3);

// Ejercicio 2

String.prototype.head = function() {
    return this[0];
}

String.prototype.tail = function() {
    return this.substr(1, this.length - 1);
}

let hello = "hola";
console.log(hello.tail() == "ola");
console.log(hello.head() == "h");
console.log(hello == "hola");

// Ejercicio 3
function Estado(esFinal, transiciones) {
    this.esFinal = esFinal;
    this.transiciones = Object.assign({}, transiciones);

    this.acepta = function (s) {
        if (s === "") {
            return this.esFinal;
        } else if (this.transiciones[s.head()]) {
            let aux = s.head();
            let aux2 = s.tail();
            return this.transiciones[s.head()].acepta(s.tail());
        }
        return false;
    }
}
let dum = new Estado(false, {});

Object.setPrototypeOf(q1, dum);
Object.setPrototypeOf(q2, dum);
Object.setPrototypeOf(q3, dum);
Object.setPrototypeOf(qf, dum);

console.log(qf.acepta(""));
console.log(qf.acepta("aaa") === false);

console.log(q1.acepta("aaa") === false);
console.log(q1.acepta("ab"));

// Ejercicio 4
Estado.prototype.nuevaTransicion = function (etiqueta, destino) {
    this.transiciones = Object.assign({}, this.transiciones);
    this.transiciones[etiqueta] = destino;
}

let q4 = Object.create(q2);
console.log(q4.acepta("c"));
q4.nuevaTransicion("b", q1);
console.log(q4.acepta("bc"));
// Agrego nueva transicion (letra b) a q4, como q4 se creo a partir de q2, q2 NO deberia
// tener esa nueva transición
console.log(q2.acepta("ba") == false);

// Ejercicio 5
function algunoAcepta(s, qs) {
    if (Array.isArray(qs)) {
        return qs.some((elem) => elem.acepta(s))
    } else {
        return qs.acepta(s);
    }
}

console.log(algunoAcepta("c", [q1, q2]));
console.log(algunoAcepta("bab", [q1, q2]) === false);
console.log(algunoAcepta("aaaaaaaaaaab", [q2, q1]));
console.log(algunoAcepta("aaaaaa", [q1]) === false);

// Ejercicio 6
Estado.prototype.nuevaTransicionND = function (etiqueta, destino) {
    if (!this.transiciones[etiqueta]) {
        this.nuevaTransicion(etiqueta, destino);
    } else {
        if (Array.isArray(this.transiciones[etiqueta])) {
            if (!this.transiciones[etiqueta].includes(destino))
                this.transiciones[etiqueta].push(destino);
        } else if (this.transiciones[etiqueta] != destino) {
            this.acepta = function (s) {
                if (s === "") {
                    return this.esFinal;
                } else if (this.transiciones[s.head()]) {
                    return algunoAcepta(s.tail(), this.transiciones[s.head()]);
                }
                return false;
            };

            this.transiciones[etiqueta] = [this.transiciones[etiqueta], destino];
        }
    }
}

// Ejercicio 7


function esDeterministicoAux(q, visitados) {
    return Object.keys(q.transiciones)
    .every((elem) => {
            if (Array.isArray(q.transiciones[elem]))
                return false

            // no es un arreglo
            if (q.transiciones[elem] == q) {
                return true;
            }

            // apunta a otro
            if (!visitados.includes(elem))
                return true;
            else
                visitados.push(elem);

            return esDeterministicoAux(q.transiciones[elem], visitados);
    });
}

function esDeterministico(q) {
    return esDeterministicoAux(q, []);
}


q1.nuevaTransicionND("a", q2);
console.log(esDeterministico(q1) === false);
console.log(esDeterministico(q3));

function calcularResultado(){
    //Editen esta función para que devuelva lo que quieran ver. Pueden escribir acá sus tests.
    return "Ac&aacute; va el resultado.";
}
