// -*- combobulate-test-point-overlays: ((1 outline 145)); eval: (combobulate-test-fixture-mode t); -*-
go func() {
	ch <- i
	close(ch)
}()
