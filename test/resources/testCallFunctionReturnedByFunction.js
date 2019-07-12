func = function() {
    return function() {
        return 1;
    }
};

auxFunction = func();

ans = auxFunction();