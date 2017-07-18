'use strict';

Object.defineProperty(exports, '__esModule', {
	value: true
});

var _createClass = (function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ('value' in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; })();

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError('Cannot call a class as a function'); } }

var Hamming = (function () {
	function Hamming() {
		_classCallCheck(this, Hamming);
	}

	_createClass(Hamming, [{
		key: 'compute',
		value: function compute(s1, s2) {
			console.log('s1.lenght !== s2.lenght', s1.lenght !== s2.lenght);
			if (s1.length !== s2.length) {
				throw new Error('DNA strands must be of equal length.');
			}
			var count = 0;
			for (var i = s1.length - 1; i >= 0; i--) {
				if (s1[i] !== s2[i]) {
					count += 1;
				}
			}
			return count;
		}
	}]);

	return Hamming;
})();

exports['default'] = Hamming;
module.exports = exports['default'];