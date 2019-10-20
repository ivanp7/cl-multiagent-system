# cl-multiagent-system

*cl-multiagent-system* is a "multi-multithreaded" model of an multiagent 
system -- it uses green threads over native threads to simulate massive
parallel execution.

## Usage                                                           

### Entity (synchronized data structure)

A synchronized entity is defined with the macro         

```lisp                                                            
(define-entity entity-type lambda-list
               (&key declarations initialization) &rest accessors)
```                                                                

where:

* `entity-type` -- name of a new type;

* `lambda-list` -- lambda list of an instance constructor function;

* `declarations` -- list of declarations for an instance constructor function;

* `initialization` -- list of forms to be executed in an instance constructor 
after an instance has been constructed and `self` has been set;

* `accessors` -- entity accessors descriptions of the syntax 
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
                      [:calls ({accessor-name}*)]
                      [:visibility {:public | :private}]
                      [:declarations ({declaration}*)]
```

The full form allows to specify custom accessor name, additional parameters,
visibility from outside (should the corresponding macro be defined), and
used resources for the synchronization mechanism.

`:reads` and `:writes` lists help the implementation to determine which
accessors can run in parallel, and which cannot.

`:calls` list specifies which other accessors are used in this accessor body, 
to automatically extend the `:reads` and `:writes` lists.

`:visibility :public` declaration generates a macro for convenient
outside access to the accessor (the default behavior). 
`:visibility :private` accessors are considered private interface and don't
have macros to be used outside.

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

Also, there is a dynamic way of accessing entities using

```lisp
(entity-accessor name entity &rest args)
```

and

```lisp
(setf (entity-accessor name entity &rest args) value)
```

where `name` is an evaluated expression which must return name of accessor 
from description in `define-entity`.

Initialization and accessors body code can use `self` binding, which is
assigned to the entity instance itself.

### Native thread

#### Generic thread

A native thread is defined with the macro         

```lisp                                                            
(define-thread thread-type lambda-list
               (&key declarations initialization body) &rest accessors)
```                                                                

The syntax and result is identical to that of `define-entity` with the
following additions:

* `body` is a body of a lambda function which is defined in an environment
where not any of `lambda-list` bindings is available, only `self` can be
accessed; this lambda function will be used as a thread function;

* `running-p`, `start,` `stop` and `destroy` accessors 
(reading of the thread state, starting, stopping and destroying the thread, 
respectively) will be generated automatically.

#### Multiagent thread

A multiagent thread is a native thread designed to run multiple agents
using green threads. It is created with function `make-multiagent-thread`,
which takes no arguments. The only methods of it are:

* `(multiagent-thread-number-of-agents thread)` -- number of currently running
agents;

* `(multiagent-thread-map-agents thread fn)` -- map provided function over
all currently running agents.

### Agent

An agent is created with function

```lisp
(make-agent instance-id &key role-id 
    (loop-fn (funcall *agent-loop-fn-ctor* instance-id role-id)) 
    (start-fn (funcall *agent-start-fn-ctor* instance-id role-id)) 
    (stop-fn (funcall *agent-stop-fn-ctor* instance-id role-id))
    (data-table (funcall *agent-data-table-ctor* instance-id role-id))
    host-thread)
```

An agent wraps a green thread and contains associated action functions 
and data.  Agents can be started, stopped and killed, can receive/send messages
from/to other agents.

List of agent accessors and methods:

* `(agent-instance-id agent)` -- agent instance ID;

* `(agent-role-id agent)` -- agent role ID;

* `(agent-loop-fn agent)` -- (setfable) the main agent function that is called 
in a loop during execution, an agent itself is passed to the function as
a parameter;

* `(agent-start-fn agent)` -- (setfable) function called before entering
execution loop, being given an agent as a parameter;

* `(agent-stop-fn agent)` -- (setfable) function called after exiting
execution loop, being given an agent as a parameter;

* `(agent-data-table agent)` -- (setfable) associated data table;

* `(agent-host-thread agent)` -- (setfable) multiagent thread for the agent
to be executed in;

* `(agent-running-p agent)` -- agent execution status;

* `(agent-message agent &key keep)` -- (setfable) a message passed to the
agent.

List of agent methods:

* `(agent-start agent)` -- start agent execution;

* `(agent-stop agent)` -- stop agent execution.

### Messenger

A messenger is an universal inter-agent communication interface, 
designed to deliver messages to local and remote agents (i.e. not located in
the current process).

Messenger provides `(messenger-send messenger id msg)` operation,
which takes recipient agent ID and message, and returns `t` or `nil` in the 
case of success and fail, respectively. For this to work, messenger requires 
three registries: `site-registry`, `agent-registry` and `sender-registry`.

`site-registry` holds entries of the form `<agent ID>:<site>`, where `site` is
either `nil` or an agent location designator. Nil value means agent is local
and can accept messages directly. `agent-registry` holds local agents by
their IDs, and `sender-registry` holds site-specific sender functions by
location designators.

A messenger is constructed using function

```lisp
(make-messenger &key (site-registry (make-registry)) 
                     (agent-registry (make-registry))
                     (sender-registry (make-registry)))
```

The associated registries are accessed with `messenger-site-registry`,
`messenger-agent-registry`, `messenger-sender-registry`, respectively.

#### Stream sender and receiver

The library provides constructor functions for message senders and receivers 
which communicate over streams.

A sender is made with

```lisp
(make-stream-sender serializer stream lock)
```

where `serializer` is a function which takes recipient id, message and buffer 
and returns an updated buffer with serialized id and message in it.

A receiver is made with

```lisp
(make-stream-receiver deserializer stream lock 
                      &optional (buffer-size *default-buffer-size*)
                      (buffer-element-type *default-buffer-element-type*))
```

where `deserializer` is a function which takes buffer with received data
(probably incomplete or excess) and returns 3 values -- 
`id`, `msg`, `deserialized-length` (exact used length of the buffer). 
Return values may be `nil` in case of incomplete input data.

### Miscellaneous

#### Missing value

Sometimes, it is convenient to have a special constant, which
represents the absence of a meaningful value. This library provides such
a constant: `+no-value+` of type `missing-value`. This value can be tested
for with a function `no-value-p`.

#### Registry

A registry is a thread-safe wrapper of hash table.
It is created using function 

```lisp
(make-registry &optional default-value (value-test #'equal))
```

where: `default-value` -- value returned when accessing non-existent
entry; `value-test` -- function used to test equality of the new value with
the default value in `setf`-accessor (needed to decide if the entry should
be deleted).

Registry provides the following accessors and operations:

* `(registry-default-value registry)` -- read/write accessor for 
`default-value`;

* `(registry-value-test registry)` -- read/write accessor for `value-test`;

* `(registry-entry registry key &optional (default +no-value+))` -- 
read/write accessor for a value denoted by `key` (if there is no such
key, and if `default` is supplied, it is returned, otherwise `default-value`
is returned);

* `(registry-keys registry)` -- get list of all entry keys in the registry;

* `(registry-entry-present-p registry key)` -- test if an entry with such key
is in the registry;

* `(registry-map-entries registry fn)` -- call `fn` for each 
entry key and value of the entry;

* `(registry-add-entry registry key value)` -- add entry to the registry;

* `(registry-del-entry registry key)` -- delete entry from the registry.

#### Synchronized queue

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

