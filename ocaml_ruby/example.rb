def rec_factorial (i)
    if i <= 1 then i else i * rec_factorial (i - 1) end
end

def loop_factorial (i)
    x = 1
    while i > 0
        x = x * i
        i = i - 1
    end
    x
end

def fib(i)
    a = 0
    b = 1
    while i > 2 do
      c = a + b
      a = b
      b = c
      i = i - 1
    end
    if i == 1 then 0 else b end
end

puts ("Waiting for input: ")
x = readi();

puts ("Recursive factorial: ")
puts (rec_factorial(x))

puts ("Loop factorial: ")
puts (loop_factorial(x))

puts ("Loop fibonacci: ")
puts (fib(x))