//==============================================
// Program.fs
// Author: Tyler Gerritsen
// Version: 10-28-18
// Simulates a simple calculator using a stack
//==============================================

let AssertEquality inputTuple =
  match inputTuple with
  |(a,b) when a=b -> printfn "Test Success"
  |_ -> printfn "Test Fail"

//==============================================
//Build a stack based calculator
//==============================================

//==============================================
// Define types
// ==============================================

//name a stack
type Stack = StackContents of float list

//### now test the code
let testStack1=StackContents [1.0;2.0;3.0]
let (StackContents contents)=testStack1 //get the content from a stack using pattern matching
printf "T1..."
AssertEquality (contents, [1.0;2.0;3.0])

// ==============================================
// Stack primitives -- push pop
// ==============================================

// Push a value on the stack
//(StackContents contents) is a Stack var being passed over as func input
let push x (StackContents contents) =  
   StackContents (x::contents)

//### test the code
let emptyStack = StackContents []
let stackWith1test = push 1.0 emptyStack
let stackWith2test = push 2.0 stackWith1test
printf "T2..."
AssertEquality (stackWith2test, StackContents [2.0;1.0])

//push takes x and a stack as input variables. We can give x as input var only and have f (defined-x, stack) as return value
let ONE = push 1.0
let TWO = push 2.0
let THREE = push 3.0
let FOUR = push 4.0
let FIVE = push 5.0
let EMPTY = StackContents []

//### test the code
let stackWith1 = ONE EMPTY
let stackWith2 = TWO stackWith1
let stackWith3  = THREE stackWith2

// some concise pipelining tests:
let result123 = EMPTY |> ONE |> TWO |> THREE
printf "T3..."
AssertEquality (stackWith3, result123)

// testing again:
let result312 = EMPTY |> THREE |> ONE |> TWO
printf "T4..."
AssertEquality (result312,StackContents [2.0;1.0;3.0])

// Pop a value from the stack and return it
// and the new stack as a tuple
let pop (StackContents contents) =
   match contents with
   |top::rest ->
     let newStack = StackContents rest
     (contents.Head, newStack)
   |[] ->
       failwith "Stack underflow"

//### testing code
let initialStack = EMPTY  |> ONE |> TWO
let popped1, poppedStack = pop initialStack
let popped2, poppedStack2 = pop poppedStack
printf "T5..."
AssertEquality (initialStack,StackContents [2.0;1.0])
printf "T6..."
AssertEquality (poppedStack,StackContents [1.0])
printf "T7..."
AssertEquality (popped2,1.0)


// ==============================================
// Operator core
// ==============================================
//let us do add first:
let ADDtest stack =
 let x,s = pop stack  //pop the top of the stack
 let y,s2 = pop s     //pop the result stack
 let result = x+y   //do the math
 push result s2       //push back on the doubly-popped stack

//###test the code
let add12=ADDtest initialStack //list is immutable, so the initialStack is always [2.0;1.0]
printf "T8..."
AssertEquality (add12, StackContents [3.0])

// define MUL
let MULtest stack =
 let x,s = pop stack //pop the top of the stack
 let y,s2 = pop s     //pop the result stack
 let result = x*y   //do the math
 push result s2      //push back on the doubly-popped stack

//###test the code
let mul12=MULtest initialStack
printf "T9..."
AssertEquality (mul12, StackContents [2.0])

//ADD and MUL are super similar since they both use 2 numbers for calc
//We can merge ADD and MUL together to make the code even more concise.
//Define a generic binary math operator:
// pop the top two elements
// do a binary operation on them
// push the result
let binary mathFn stack =
   let y,stack' = pop stack   
   let x,stack'' = pop stack' 
   let z = mathFn x y
   push z stack''     

//If we want to do add using this generic binary math func:
let genericBinaryMath12=binary (+) initialStack //This is same to: let ADD aStack = binary (fun x y -> x + y) aStack
//By using partial func, we have a new way of defining ADD and more:
let ADD = binary (+)
let SUB = binary (-)
let MUL = binary (*)
let DIV = binary (/)

//###Testing code:
let div3by2 = EMPTY|> THREE|> TWO|> DIV
let sub2from5 = EMPTY|> FIVE|> TWO|> SUB
let sub2from5thenadd1 = EMPTY|> FIVE|> TWO|> SUB|> ONE|> ADD

printf "T10..."
AssertEquality(div3by2, StackContents [1.5])
printf "T11..."
AssertEquality(sub2from5, StackContents [3.0])
printf "T12..."
AssertEquality(sub2from5thenadd1, StackContents [4.0])

// define a generic unary operator for power **
// pop the top element
// do a unary operation on it
// push the result
let unary f stack =
 let x,stack' = pop stack 
 push (f x) stack'        

//We can define negative number or square with the unary operator and the partial function
let NEG = unary (fun x -> -x)
let SQUARE = unary (fun x -> x**2.0)

//###Now test the code:
let neg3 = EMPTY  |> THREE|> NEG
let square2 = EMPTY|> TWO|> SQUARE
let squareneg3= EMPTY|> THREE|> NEG|> SQUARE

printf "T13..."
AssertEquality(neg3, StackContents [-3.0])
printf "T14..."
AssertEquality(square2, StackContents [4.0])
printf "T15..."
AssertEquality(squareneg3, StackContents [9.0])
// ==============================================
// Other core
// ==============================================

/// Pop and show the top value on the stack
let SHOW stack =
   let x,_ = pop stack
   printfn "The top of the current stack is %f" x
   stack  // return the same stack

/// Drop the top value on the stack
let DROP stack =
   let _,s = pop stack  //pop the top of the stack
   s                    //return the rest

/// Duplicate the top value on the stack
let DUP stack =
   // get the top of the stack
   let x,_ = pop stack 
   // push it onto the stack again
   push x stack

/// Swap the top two values
let SWAP stack =
   let x,s = pop stack 
   let y,s' = pop stack
   push y (push x s')


let CUBE =
   DUP >> DUP >> MUL >> MUL

let SUM_NUMBERS_UPTO =
   DUP                     // n 
   >> ONE >> ADD           // n+1
   >> MUL                 // n(n+1)
   >> TWO >> DIV   // n(n+1) / 2

//### One final test of the calculator
let myStack= EMPTY |> ONE |> TWO |> THREE
let stackTop= myStack |> SHOW
let stackTopEleCubed = myStack |> CUBE |> SHOW
let stackSum=myStack |> SUM_NUMBERS_UPTO |> SHOW

System.Console.ReadKey() |> ignore
