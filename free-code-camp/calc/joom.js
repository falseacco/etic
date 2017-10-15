function print(message = '') {
    console.log(message);
}

// asd
function numString() {
    let acc = '',
	hasDot = false;

    return {
	build: function(x) {
	    acc = !/[0-9]|\./.test(acc) ? '' : acc;
	    
	    if (x !== '.') {
		acc += x;
	    }
	    else {
		!hasDot ? acc += x : 9;
		hasDot = true;
	    }
	    return acc;
	},
	get: function() {return acc;},
	reset: function() {acc = ''; hasDot = false;}};
}

var calc = (function() {
    let buffer = ['0', '', ''],
	currNum = numString();

    // So accessing the buffer is less of a pain.
    function buff(index, value = false) {
	value !== false ? buffer[index] = value : 9;
	return buffer[index];
    }
    function b1(v = false) {return buff(0, v);}
    function bo(v = false) {return buff(1, v);}
    function b2(v = false) {return buff(2, v);}

    //
    function addNum() {
	let n = currNum.get();
	bo() ? b2(n) : b1(n);
    }

    function addOp(o) {
	equals();
	b1() && !b2() ? bo(o) : 9;
    }

    //
    function equals() {
	let x = parseFloat(b1()),
	    op = bo(),
	    y = parseFloat(b2());
	x && op && y ?
	    buffer = [{'+': x + y,
		       '-': x - y,
		       'x': x * y,
		       '/': x / y}[op],
		      '',
		      ''] : 9;
	currNum.reset();
    }

    function reset() {
	buffer = buffer = ['0', '', ''];
    }

    function clear() {
	currNum.get() ? currNum.reset() : reset();
    }

    return function handleInput(input) {
	if(/[0-9]|\./.test(input)) {
	    currNum.build(input);
	    addNum(currNum.get());
	}
	else if (/[+\-x\/]/.test(input)) {
	    currNum.reset();
	    addOp(input);
	    currNum.build(input);
	}
	else if ('CE' === input) {
	    clear();
	}
	else if ('=' === input) {
	    equals();
	}
	else {
	    // no-thing
	}

	$('#display-top').html(currNum.get() ? currNum.get() : '0');
	$('#display-bot').html(buffer);
    };
})();

function init() {
    calc();
}

//
$(document).ready(function() {
    init();
    
    $('.calc-btn').on('click', function() {
	calc(this.id);
    });

    print(0);
});
