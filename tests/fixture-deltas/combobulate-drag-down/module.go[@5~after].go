// -*- combobulate-test-point-overlays: ((1 outline 185) (2 outline 199) (3 outline 213) (4 outline 630) (5 outline 665) (6 outline 717)); eval: (combobulate-test-fixture-mode t); -*-
package main

import "fmt"

func main() {

	nums := []int{2, 2, 2, 2, 3, 4}
	sum := 0
        sum += num
        fmt.Println("sum:", sum)
	fmt.Println("sum:", sum)



	fmt.Println("index:", i)

        map[string]string{"a": "apple", "b": "banana"}
	for k, v := range kvs {
		fmt.Printf("%s -> %s\n", k, v)
		// Do something with k and v
	}

	for k := range kvs {
		fmt.Println("key:", k)
	}

	for i, c := range "go" {
		fmt.Println(i, c)
	}
}

var (
	foo = "bar"
	baz = "qux"
)

type Person struct {
	Name string
	Age  int
}

const (
	message = "Hello, world!"
	foo     = 42
)
