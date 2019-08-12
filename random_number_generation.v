fn random_integer(max int) int {
	return rand.next(max)
}

// https://www.geeksforgeeks.org/random-number-generator-in-arbitrary-probability-distribution-fashion/
// returns a random number from arr[] according to
// distribution array defined by freq[]. n is size of arrays.
fn random_integer_distribution(arr[] int, freq[] int, n int) int {
		// Create and fill prefix array
    mut prefix := [0; n]
    prefix[0] = freq[0]
		for i := 1; i < n; i++ {
      prefix[i] = prefix[i - 1] + freq[i]
		}
    // prefix[n-1] is sum of all frequencies. Generate a random number
    // with value from 1 to this sum
    r := (random_integer(prefix[n - 1])) + 1
    // Find index of ceiling of r in prefix arrat
  	indexc := find_ceil(prefix, r, 0, n - 1)
    return arr[indexc]
}

// Utility function to find ceiling of r in arr[l..h]
fn find_ceil(arr[] int, r int, l int, h int) int {
		mut mid := 0
		mut le := l
		mut he := l
		for {
			mid = (le + he) / 2
			if r > arr[mid] { le = mid + 1 }  else { he = mid }
			if !(le < he) { break }
		}
		mut res := 0
		if arr[le] >= r { res = le } else { res = -1 }
    return res
}
