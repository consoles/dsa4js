'use strict';

module.exports = class DoubleNode {

    constructor(value) {
        this.next = null;
        this.prev = null;
        this.value = value;
    }
};