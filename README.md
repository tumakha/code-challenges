# Intent HQ Java/Scala Code Challenges

If you are a candidate and are doing one (or more) of this small katas at home you may want to read this.

There are different katas in the package `com.intenthq.challenge`, each of them consists of a single file: `SKataName` (if scala) or `JKataName` (if java).

Each challenge comes with a set of tests written in specs2, you won't probably need to add any test, but in case you need to, you'll find documentation about the test framework in here: http://specs2.org/

## How to start

1. Read all the challenges and chose the one(s) you like best
2. Fork the repo and start working on it
3. Push your changes and send us the link to the github repo
4. If you have any doubt while working on it just drop us an email and we'll help

```
To test your scala code:
$ sbt testOnly -- include scala

To test your java code:
$ sbt testOnly -- include java

To test a specific challenge:
$ sbt testOnly -- include nice/graph -- exclude java/scala
```
