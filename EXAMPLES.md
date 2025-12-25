# Constraint Interaction

Useful examples to consider for how the constraints interact.

The notation `<LV#>` represents a logic variable.
The notation `_#` represents a more general "hole".

## (Greg) Where do logic variables come from?

Let's figure that out indirectly, through examples.

Logic variables are values, not expressions, so they do not appear directly in programs.
They are introduced by the supercompiler any time it needs to produce an unknown value
(possibly with some constraints).

For instance, if a user wants to supercompile an expression with "holes", they do so 
indirectly using lambda.  e.g., to supercompile:

```
(if (= _0 _1)
    _0
    (if (= _0 _1)
        #f
        _0))
```

they would actually supercompile:

```
(lambda (a b)
  (if (= a b)
      a
      (if (= a b)
          #f
          a)))
```

Supercompiling a lambda introduces fresh logic variables corresponding to its argument values
(because they are unknown), binds the parameters to these arguments,
then continues supercompiling the lambda body.

Here's another example involving another kind of unknown:

```
(lambda (x)
  (= (car x) 3))
```

Like the previous example, `x` will be bound to a fresh logic variable (let's call it LV0).
Next, when we try to evaluate `(car x)`, because we don't know anything about `x` yet,
we have to introduce another fresh logic variable (let's call it LV1) corresponding
to taking the `car` of LV0.
Additionally, we also need to introduce new constraints,
and wrap things with a dnode:canfail, since we don't know whether LV0 is a pair yet.  

A straightforward way to represent this situation with constraints would be to introduce yet another logic variable (call it LV2) that corresponds to the `cdr` of LV0, while adding the equality constraint that `(== LV0 (,LV1 . ,LV2))`.  By doing it this way, subsequent `(car x)` or `(cdr x)` expressions will consistently produce the same logic variable each time.
(More generally, we are choosing a constructor for `LV0`. Other constructors can work
the same way.)

A later step will constrain LV1 to be a number (also with a dnode:canfail).
LV2 is left unconstrained.

Another way fresh logic variables can be introduced is when the supercompiler
suspends a complex computation, possibly because it gives up,
or because it recognizes a recursive call, etc.

... for delayed computations. ...

## (Greg) How are logic variables residualized?

Once supercompilation is done, a residualized program needs to be produced from the result.
The result of supercompilation will be a graph whose nodes embed values,
where these values represent whatever the supercompiler was able to figure out about
a computation.  When producing the residualized program, each value will be converted
(through a trivial process) to an expression that computes that value. 
The problem is that these values will often contain, or be, logic variables.

Looking at the supercompiled lambda example, logic variables will be introduced for the lambda parameters `a` and `b` (let's call them LV0 and LV1), and the supercompiled result of the body will be something like:

```
(dnode:canfail (num? <LV0>) 
  (dnode:canfail (num? <LV1>) 
      <LV0>))
```

and the supercompiled result of the entire lambda will correspond to a closure,
with its enclosing environment (not relevant here), parameters `(a b)`,
and the supercompiled body.  Ultimately, we want to produce a residualized lambda that looks something like:

```
(lambda (a b)
  (if (number? a)
      (if (number? b)
          a
          (error "not a number" b))
      (error "not a number" a)))
```

To achieve this, we need a way to map LV0 to `a` and LV1 to `b`.

When a fresh logic variable is constructed, it will be given the expression
it corresponds to, and which it will be replaced by during residualization.  

For instance, in this same example, when we create LV0 and LV1,
they would remember that they stands for `a` and `b` respectively.
(Not just through the lexical environment: the LV constructor will have a field
corresponding to this.)

Recall this example from earlier:

```
(lambda (x)
  (= (car x) 3))
```

Here, we introduced LV0 for x, LV1 for the car of LV0, and LV2 for the cdr of LV0.
Like the previous example, LV0 would remember that it stands for `x`.
But LV1 and LV2 were introduced while evaluating an accessor on LV0,
so we need a new way to map each of them to a residualization.  

We will map LV1 to the `car` of LV0's residualization (which is `x`),
and LV2 to the `cdr` of LV0's residualization (also `x),
giving `(car x)` and `(cdr x)` respectively.  (LV2 will remain unused in this example.)

Ultimately, this example should residualize to:

```
(lambda (x)
  (if (pair? x)
      ;; TODO: we should let-bind a fresh lexical variable to (car x) to to avoid recomputing it
      (if (number? (car x))
          (= (car x) 3)
          (error "not a number" (car x)))
      (error "not a pair" x))
```

## Simple equality, no LV, barely any constraints

Example:

```
(= 4 3)
```

- First, create a stack of frames. This will look like
  ```
  ((frame (quote 4) () ()          ENV)
   (frame =         () ((quote 3)) ENV)
   (frame.halt))
  ```
- The quote evaluations do not require any constraint checks
- The only part where constraints are checked are the numeric type
  constraints implicit in `=`.
    Thus, constraint-checking should work with non-LV values,
    e.g. (check (has-type num?) (quote 4) constraint.empty)
                                -------- expression or value?

## Equality with 2 LV

Example:

```
(= <LV0> <LV1>)
```

- TODO: Normalization?

- TODO: come up with examples so that "Lexical var to logic var"
  conversion is more explicit and consistent
  - For instance, instead of (= (car <LV0>) 3), we would have 
    (= (car x) 3) in an ENV that binds x to <LV0>

## Equality with LV

Example:

```
(= (car <LV0>) 3)
```

This one is more complicated. What intermediate fresh LVs should be created?

- The initial stack of frames will look like
  ```
  ((frame quote <LV0> () ()  ENV)
   (frame car () ()          ENV)
   (frame =   () ((quote 3)) ENV)
   (frame.halt))
  ```
- This first logic variable evaluation is simple. No new constraint added to ENV
  ```
  ((frame car (<LV0>) ()     ENV)
   (frame =   () ((quote 3)) ENV)
   (frame.halt))
  ```
- When evaluating the `car` frame, a new logic variable will appear
  ```
  ((frame quote (3)   () ENV)
   (frame =   (<LV1>) () ENV^)
   (frame.halt))
  ```
  Where `ENV^` will need to contain the constraint that `<LV2>`
  equates to `(car <LV0>)`, or that `<LV0> = (<LV1> . <LV2>)`.

  Currently, this is represented using the constraint
  ```
  { <LV1> : (), <LV2>: (), <LV0>: ((= (<LV1> . <LV2>))) }
  ```
- The next step is straightforward --- though we are glossing over
  the need to combine two sets of constraints from two environments.
  In this case, the environment merging produces ENV^ since 3 is just 3.
  ```
  ((frame =   (<LV1> 3) () ENV^)
   (frame.halt))
  ```
- Now, evaluating the `=` requires working with constraints.
  We will need to check/verify that `<LV1>` is a number, and `3` is a number.
  e.g. something like
  ```
  (check (has-type num?) <LV1> CX-FROM-ABOVE)
  ```
  This should succeed, with the new set of constraints
  ```
  { <LV1> : ((has-type num?)), <LV1>: (), <LV0>: ((= (<LV1> . <LV2>))) }
  ```

## If with equality constriants

Example:

```
(if (= <LV0> <LV1>)
  <LV0>
  (if (= <LV0> <LV1>)
    #f
    <LV0>))
```

This should supercompile to just `<LV0>`.

- Top-level `dnode:canfail` if `<LV0>` and `<LV1>` are not of the type num?
- The first `if` expression will generate an "dnode:if" node.
- The two branches will have different associated constraints
    - first branch: `{ <LV0> : ((has-type num?) (= <LV1>)), <LV1>: ((has-type num?)) }`
    - second branch: `{ <LV0> : ((has-type num?) (not-= <LV1>)), <LV1>: ((has-type num?))}`
- The "true" branch will just have a transient node
- The "false" branch will (should?) simplify when the condition expression
  is tested in the dnode:canfail smart constructor,
  and produce a transient node with just `<LV0>`
  and associated constraints
- A simplificiation step should see that the two branches of the `dnode:if`
  are the same and return a transient node instead.

=> 
(dnode:canfail (num? <LV0>) 
    (dnode:canfail (num? <LV1>) 
        <LV0>))


## calls?

Should we think about calls sooner rather than later?

Example:

```
(letrec ((len (lambda (lst)
                (if (pair? lst)
                  (cons (quote 1) (call len (cdr lst))) ; todo change cons => +
                  (quote 0)))))
  (call len (cons (quote 1) (cons (quote 0) (cons (quote 1) (quote ()))))))
```




