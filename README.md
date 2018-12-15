#### Genetic programming playground

Basic GA&GP related concepts can be found here:
http://www.myreaders.info/09_Genetic_Algorithms.pdf

##### HOW TO USE:
- Download, install lein and Gorilla, cd to the directory and $ Lein gorilla 
- Load target clj and run

##### FILES:
- abs.clj -> A simple GP test on finding absolutebvalue function. \
It's harder to make a GP program generate simple functions, because it's usually hard to write their fitness functions.
- ast.clj -> A GP program which tries to classify astronomical data and predict what type a given input should be. 
 \
-> CURRENT STATE OF THE PROGRAM: 
The total error gets too big that the program sometimes thinks it’s better to have no-stack-item or same behaviors all the time rather than to have the in1 instruction. 
Dec 14th, 2018 => Just noticed that there's a problem in the current error-granting method under extreme situations.
 \
-> Future directions
    1. Modify the fitness function
    Current concerns: Based on the current performance, it's not likely the program is going to find out how to round off and take modulo of the result. Some other fitness functions might be needed. 
    2. Use Lexicase to selects parents that are good at predicting a particular category use these programs to generate a combination that can behave generally good on predicting all categories
    3. Find some way to balance data scale (with pregiven artificial methods..)-> to make numbers that are really small to matter equally as those with astronomical scale
    4. Modify cases in lexicase
- other files -> helper functions



Structural reference:\
-> Adaptive Probabilities of Crossover and Mutation in Genetic Algorithms\
https://pdfs.semanticscholar.org/7f62/05f1cac497e9929cc2c7f6f1c767c2229254.pdf 

Crossover\
-> https://en.wikipedia.org/wiki/Crossover_(genetic_algorithm) \
-> https://www.tutorialspoint.com/genetic_algorithms/genetic_algorithms_crossover.htm


Mutation\
-> https://en.wikipedia.org/wiki/Mutation_(genetic_algorithm) \
->IMPROVING MUTATION CAPABILITIES IN A REAL - CODED GENETIC ALGORITHM\
 http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.41.6687&rep=rep1&type=pdf \
-> Evolutionary Programming Based on Non-Uniform Mutation
