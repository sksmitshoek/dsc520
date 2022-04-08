say.hello <- function()
{
  print("Hello, world!")
}

say.hello

sprintf("Hello %s", "Jared")
sprintf("Hello %s, today is %s", "Jared", "Sunday")

hello.person <- function(name)
{
  print(sprintf("Hello %s", name))
}

hello.person("Jared")
hello.person("Bob")

hello.person <- function(first, last)
{
  print(sprintf("Hello %s %s", first, last))
}

hello.person("Stephen", "Smitshoek")

hello.person <- function(first, last="Doe", ...)
{
  print(sprintf("Hello %s %s", first, last))
}

hello.person("Bob", "Smith", "Hello")

double.num <- function(x)
{
  return(x * 2)
  
  print("Hello!")
}

double.num(5)

run.this <- function(x, func=mean)
{
  do.call(func, args=list(x))
}

run.this(1:10, sum)



