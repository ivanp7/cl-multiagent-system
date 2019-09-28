# cl-multiagent-system

*cl-multiagent-system* is a multithreaded model of an multiagent system.

## Usage                                                           

### Entities

A synchronized entity is defined with the macro         

```lisp                                                            
(define-synchronized-entity entity-type lambda-list
                            (&key declarations initialization)
                            &rest accessors)
```                                                                

where:

* `entity-type` -- name of a new type;

* `lambda-list` -- lambda list of an instance constructor function;

* `declarations` -- list of declarations for an instance constructor function;

* `initialization` -- list of forms to be executed in an instance constructor 
after an instance has been constructed and `self` has been set;

* `accessors` -- entity accessors descriptors of the syntax 
in one of two forms:

```
// short form:
accessor ::= resource-name | (setf resource-name)
```

The short form defines a simple, single resource, 
synchronized public getter/setter, which take no additional arguments.

```
// full form:
accessor ::= ((accessor-name accessor-lambda-list [accessor-settings]) 
              {body-form}*)

// getter
accessor-name ::= symbol
accessor-lambda-list ::= ({ordinary-lambda-list})

// setter
accessor-name ::= (setf symbol)
accessor-lambda-list ::= (new-value-var {ordinary-lambda-list})

accessor-settings ::= [:writes ({name}*)] [:reads ({name}*)] 
                      [:visibility {:public | :private}]
                      [:declarations ({declaration}*)]
```

The full form allows to specify custom accessor name, additional parameters,
visibility from outside (should the corresponding macro be defined), and
used resources for the synchronization mechanism.

`:reads` and `:writes` lists help the implementation to determine which
accessors can run in parallel, and which cannot.

From within an entity, all accessors are available as local functions 
(defined using `labels`), regardless to their visibility.

Accessor's `:declarations` are applied to a corresponding local function.

The macro defines:

* function `make-<entity-type>` -- an instance constructor 
(name generator can be replaced: set `*constructor-name-fn*`);

* macros `<entity-type>-<getter-name>` -- getters 
(name generator can be replaced: set `*accessor-name-fn*`);

* setf expansions `<entity-type>-<setter-name>` -- setters 
(name generator can be replaced: set `*accessor-name-fn*`).

Initialization and accessors body code can use `self` binding, which is
assigned to the entity instance itself.

### Agents

The library includes `define-agent` macro and ready `dagent` (dynamic agent) 
facility to help develop multiagent system models. 
An agent wraps a thread object and contains
associated action functions and data.  Agents can be started, stopped 
and killed, can receive/send messages from/to other agents.

The code in `agent.lisp` is self-documentary.  The interested reader 
is invited to get acquainted with the implementation themselves.

### Synchronized queues

As a bonus, the library provides public interface to synchronized queues.  
A queue are protected with a mutex. 

List of supported operations (see `queue.lisp` for details):

* testing for emptiness;

* accessing first and last elements;

* pushing and popping multiple elements.

## Author                                                          

Ivan Podmazov (ivanpzv8@gmail.com)                                 

## [License](LICENSE)                                              

Copyright (c) 2019 Ivan Podmazov                                   

