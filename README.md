SocialNetworkAnalysis
=====================

Numerous complex systems such as social networks can be modeled using graphs. Graphs used in social network mining are generally made up of numerous nodes (Facebook 1.15 billion, YouTube 1 billion on september 2013) and/or are very low dense. If the available architectures are not efficiently exploited, certain algorithms will have very long execution times. However, the evolution of architectures from mono-core through multi-cores to many-cores makes them complex thus rendering difficult the task of writing programs to be run on these machines.   

Looking for a language that can associate both facility and efficiency, we start proposing a DSL (Domain Specific Language) written and embedded in Erlang. This DSL should facilitates the programming of social network mining algorithms on multi-core and many-core platforms. So that writing parallel code on these platforms should be an easy task.   

In this file, we give some functions that should be useful for a social network analyzer (load_graph_gml ...). We propose an extra-operator (parallel_op) that help him to easily write parallel code without specifying messages and process.   We will be happy to know that Erlang  community  will accept and continuous this project. Note that this kind of project is already done in Python. But because of its GIL (Global Interpreter Lock), Python doesn't take profit to the available multi/many-core to reduce execution time.

We will be happy to know that Erlang  community  will accept and continuous this project. Note that this kind of project is already done in Python. But because of its GIL (Global Interpreter Lock), Python doesn't take profit to the available multi/many-core to reduce execution time.

Thomas Messi Nguélé.
