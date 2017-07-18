"use strict";

Object.defineProperty(exports, "__esModule", {
	value: true
});

exports["default"] = function (year) {
	if (year % 400 === 0 && year % 4 === 0) {
		return true;
	}

	if (year % 100 === 0 && year % 4 === 0) {
		return false;
	}

	if (year % 4 === 0) {
		return true;
	}

	return false;
};

module.exports = exports["default"];