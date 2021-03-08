# 🚒✨ Rescue
## More Understandable Error Handling

![Continuous Integration](https://github.com/fission-suite/fission/workflows/Continuous%20Integration/badge.svg)
![License](https://img.shields.io/github/license/expede/rescue)

`rescue` is an experimental error handling library for Haskell.

The standard library approach to error handling is to use an existential type to
_subclass_ any error as `SomeException`. This is very convenient 
(requires nealy zero set up), and matches what  developers expect coming from 
other ecosystems.

A core goal of `rescue` is to give the programmer clarity about which execptions
are possible at any given point in the code. We achieve this by using (open) variants
for compositon rather than inheritance. We hide the detail as much as posisble,
and attempt to make this work in a flexible, constraint-driven style as much 
as possible.

Perhaps there is some value in treating these more separately. 
There is an old distinction that mirrors the above as errors (irrecoverable) 
vs exceptions (recoverable).

`rescue` breaks the problem into synchronous and asynchoronous exceptions.
This distinction allows us to express our assumptions about the error 
environment, separate modes of handling for the same monadic stack, 
and even different errors.

# Goals

Our goals are to have:

* Clarity of what errors can be raised by some code
* Not need a large tree of error types for our application (set-like)
* Way to declare assumptions about which errrors are available
  * e.g. "can raise X, Y, and Z", or "raises exactly X and Y but none others"
* Extensible
* Ability to call code that raises some subset of errors
* Handle (and eliminate) exceptions from the context

# Approach

The closest approach is `MonadError`, except that our implementation uses 
type-level lists for flexiblity,  and hides the exception parameter from the 
class constraint as a type family.  `rescue` also splits `raise` and `attempt` 
into separate classes to help us more granularly express the effects available 
in the current context. 

## Class Heirarchy

1. `MonadRaise` -- roughly `MonadThrow`
2. `MonadRescue` -- roughly `MonadCatch`
3. `MonadCleanup` -- roughly `MonadBracket`

`Monad m => MonadRaise m => MonadRescue m => MonadCleanup m`

## Errors / Asynchronous Exceptions

An asynchronous error (e.g. coming from another thread)
comes with a lot more uncertainty. We may not be aware of the type of error
being thrown to us, or when it's thrown (since the runtime will interrupt).
The purpose of this scenraio is typically to cleanup some resource 
and immeditely rethrow.

While a more structured, type-driven style would be appreciated here, this is
fundamentally how the runtime works. As such, we do need to contend 
with `SomeAsyncException`s.

The good news is that we're not expected to do much with them. Essentially all
we care about is:

* _That_ an async exception interrupted our flow, 
* We should not attempt recovery
* The error should be rethrown

Cleanup is also a use case for synchronous exceptions. If something is thrown
inside a context with an open resource, we should clean that up.

The basic flow for an async execption is then:

1. An async exception is raised
2. Convert it to a synchronous exception 
3. Run the normal cleanup
4. Rethrow the original asynchronous error

This means that any instacne of MonadAsyncCleanup needs a MonadCleanup and `Raises m SomeAsyncException`

# FAQ

## Why another typeclass?

It's true that MonadThrow is totally pervasive. However, not trying to 

There's also nothing stopping us from writing a function of the type

```haskell
fromThrow :: (MonadCatch m, MonadRaise n) => m a -> n a
```

...though the conversion from `SomeException` would take a bit of care.

## Why avoid `SomeException`?

`SomeException` is typesafe, but very difficult to track by hand.
In fact, with async exceptions, you may need to handle an error interrupting
your execution _even if you don't have a `MonadThrow`_ in your context.

By treating exceptions as something visible and tarckable (though hidden
when you don't need it), we gain a lot of ability to reason about our program,
and avoid writing lots of nested `Either`s.

## Does this do async exception handling?

It does! `MonadCleanup` is the typeclass, and it has very few instances.
It's essentially [`MonadBracket`](https://www.fpcomplete.com/blog/2017/02/monadmask-vs-monadbracket/),
but with explicit errors.

This separation of the `SomeException` and `OpenUnion` can help determine the
intention of the exception. `SomeException` (and `SomeAsyncException`) are really
more like errors -- things that should fail and not be recovered from. In a world
with async exceptions, the cleanest way to respond to an error is to stop
our execution, cleanup any resources, and propogate the error.

## Are there other packages attempting to solve this problem?

Yep! For instance, [`safe-exceptions`](https://hackage.haskell.org/package/safe-exceptions).
They distinguish between three kinds of exceptions:

> We're going to define three different versions of exceptions. Note that these definitions are based on how the exception is thrown, not based on what the exception itself is:
>
>    **Synchronous** exceptions are generated by the current thread. What's important about these is that we generally want to be able to recover from them. For example, if you try to read from a file, and the file doesn't exist, you may wish to use some default value instead of having your program exit, or perhaps prompt the user for a different file location.
>
>    **Asynchronous** exceptions are thrown by either a different user thread, or by the runtime system itself. For example, in the async package, race will kill the longer-running thread with an asynchronous exception. Similarly, the timeout function will kill an action which has run for too long. And the runtime system will kill threads which appear to be deadlocked on MVars or STM actions.
>
>    In contrast to synchronous exceptions, we almost never want to recover from asynchronous exceptions. In fact, this is a common mistake in Haskell code, and from what I've seen has been the largest source of confusion and concern amongst users when it comes to Haskell's runtime exception system.

>    **Impure** exceptions are hidden inside a pure value, and exposed by forcing evaluation of that value. Examples are error, undefined, and impureThrow. Additionally, incomplete pattern matches can generate impure exceptions. Ultimately, when these pure values are forced and the exception is exposed, it is thrown as a synchronous exception.
>
>    Since they are ultimately thrown as synchronous exceptions, when it comes to handling them, we want to treat them in all ways like synchronous exceptions. Based on the comments above, that means we want to be able to recover from impure exceptions.

...along with some general guidance...

>   All synchronous exceptions should be recoverable
>   All asynchronous exceptions should not be recoverable
>   In both cases, cleanup code needs to work reliably

Rescue is primarily focused on deligering a great experince when dealing with 
synchronous excpetions, though touches on async ones as well. 
