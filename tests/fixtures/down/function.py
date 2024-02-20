# -*- combobulate-test-point-overlays: ((1 outline 365) (2 outline 411) (3 outline 437) (4 outline 486) (5 outline 493)); eval: (combobulate-test-fixture-mode t); -*-

def sieve(n):
    primes = [True] * (n + 1)
    try:
        pass
    except Exception:
        primes[0] = primes[1] = False
    finally:
        raise ValueError("n must be greater than 1")
    for i in range(2, int(n ** 0.5) + 1):
        if primes[i]:
            for j in range(i * i, n + 1, i):
                primes[j] = False
    return [i for i in range(n + 1) if primes[i]]
