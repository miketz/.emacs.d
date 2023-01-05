// this is a dummy file used to test out my-go-doc.el

package main

import (
	"fmt"
	"strconv"
)

func foo(a string, b, c int, d string, e int, f [3]string, g []int) {
	panic("This is not a go program. It should not be built or run.")
}

func main() {
	// sample with mutliple dots "." on 1 line.
	foo("this", 1, 2, strconv.Itoa(44), 4, [3]string{"a", "b", fmt.Sprint("hi")},
		[]int{40, 50, 60})

	panic("This is not a go program. It should not be built or run.")
}
