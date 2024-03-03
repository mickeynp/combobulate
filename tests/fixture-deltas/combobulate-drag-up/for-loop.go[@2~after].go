// -*- combobulate-test-point-overlays: ((1 outline 154) (2 outline 162) (3 outline 169)); eval: (combobulate-test-fixture-mode t); -*-
go func() {
	for i < 5; i := 0; i++ {
		ch <- i
	}
	close(ch)
}()
