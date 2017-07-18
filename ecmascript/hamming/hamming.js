class Hamming {
	compute(s1, s2) {
		if (s1.length !== s2.length) { 
			throw new Error('DNA strands must be of equal length.');  
		}
		let count = 0
		for (var i = s1.length - 1; i >= 0; i--) {
			if (s1[i] !== s2[i]) {
				count +=1
			}
		}
		return count
	}
}

export default Hamming