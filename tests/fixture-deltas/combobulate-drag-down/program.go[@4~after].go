// -*- combobulate-test-point-overlays: ((1 outline 201) (2 outline 215) (3 outline 261) (4 outline 313) (5 outline 363) (6 outline 620) (7 outline 667)); eval: (combobulate-test-fixture-mode t); -*-
package main

import (
	"fmt"
	// foo
	"example.com/foo"
)

const (
	message = "Hello, world!"
	foo     = 42
)

func main() {
	ch := make(chan int)

	go func() {
		for i := 0; i < 5; i++ {
			ch <- i
		}
		close(ch)
	}()

	go func() {
		for {
			if val, ok := <-ch; ok {
				fmt.Println("Received from channel:", val)
			} else {
				break
			}
		}
	}()

	select {}
}

var (
	globalVar1 = 42
	globalVar2 = "example"
)

type Person struct {
	Name string
	Age  int
}

func main() {
	var localVar int
	localVar = 5

	fmt.Println(message)
	fmt.Println(globalVar1)
	fmt.Println(globalVar2)

	person := Person{Name: "Alice", Age: 30}
	fmt.Println(person)

	for i := 0; i < 3; i++ {
		fmt.Println("Iteration:", i)
	}

	switch localVar {
	case 1: {
		fmt.Println("One")
	}
	case 2:
		fmt.Println("Two")
	default:
		fmt.Println("Default")
	}

	if localVar > 3 {
		fmt.Println("Greater than 3")
	} else {
		fmt.Println("Less than or equal to 3")
	}
}
