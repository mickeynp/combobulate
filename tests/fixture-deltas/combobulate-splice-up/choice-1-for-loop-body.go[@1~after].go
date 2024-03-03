// -*- combobulate-test-point-overlays: ((1 outline 145)); eval: (combobulate-test-fixture-mode t); -*-
go func() {
	for i := 0; i < 5; i++ ch <- i
	close(ch)
}()
