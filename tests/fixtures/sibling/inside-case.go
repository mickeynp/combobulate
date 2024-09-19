// -*- combobulate-test-point-overlays: ((1 outline 151)); eval: (combobulate-test-fixture-mode t); -*-
switch value := x.(type) {
case int:
	// Foo
	fmt.Println("Integer type")
        go func() {
		for i := 0; i < 5; i++ {
			ch <- i
		}
		close(ch)
	}()
	fmt.Println("Integer type")
case string:
        fmt.Println("String type")
default:
        fmt.Printf("Unexpected type %T\n", value)
}

