# Scala Retro
This package allows to generate UML Class Diagrams from source code.

The design allows for various extensions in terms of ways to tokenize a source code (different languages for example) as well as ways to serialize the output diagrams.

Currently, only Scala language source codes can be tokenized and outputing is performed through the generation of [Mermaid.js](https://mermaid-js.github.io) compatible markdown files.

## Getting Started

### Install

To begin using the package, setup your `build.sbt` by adding the following for the latest version:

```scala
resolvers += Resolver.bintrayRepo("acout", "maven")
libraryDependencies += "com.github.acout" %% "scala-retro-core" % "0.1.4"
//Optional: only to use the default CLI
libraryDependencies += "com.github.acout" %% "scala-retro-cli" % "0.1.4"
```

### Basic Diagram Generation With Code

```scala
import com.github.acout.scalaretro.core.tokenizer.ScalaTokenizer
import com.github.acout.scalaretro.core.writer.MermaidClassDiagramWriter
import com.github.acout.scalaretro.core.Utils
import java.io.{File, FileWriter}

//Retrieve all ".scala" files recursively from one root folder
val files = Utils.getAllScalaFiles(new File("some/root/folder/path").toPath)
//Tokenize the different files as a flatMapped list of Token
val tokens = new ScalaTokenizer().tokenize(files)
//Prepare the Mermaid.JS Class Diagram Writer with adequate output FileWriter
val writer = new MermaidClassDiagramWriter(new FileWriter(new File("output.md")))
//Serialize tokens as Mermaid.JS compatible markdown and write to the output file
writer.write(tokens)
//Close the connection
writer.close
```

### Diagram Generation With Code After Filtering By File Names

```scala
import com.github.acout.scalaretro.core.tokenizer.ScalaTokenizer
import com.github.acout.scalaretro.core.writer.MermaidClassDiagramWriter
import com.github.acout.scalaretro.core.filter.NameFilter
import com.github.acout.scalaretro.core.Utils
import java.io.{File, FileWriter}

val files = Utils.getAllScalaFiles(new File("some/root/folder/path").toPath)
val tokens = new ScalaTokenizer().tokenize(files)
//After generating the List[Token], filter them by class name using a regex
val nameFilter = NameFilter("^Model.*") _
//Filter and write the filtered tokens only
writer.write(tokens.filter(nameFilter))
writer.close
```

### Basic Diagram Generation With CLI, with optional Filter

```bash
#Needed to work inside the sbt environment
sbt
#The 3rd argument is optional and is the regex to match on class names for filtering
cli/run "some/root/folder/path" "output.md" "^Model.*"
```

### Obtaining the Visual Output From Generated Mermaid.js File

The final markdown content generated in output files can be copied to the [Mermaid Live Editor](https://mermaid-js.github.io/mermaid-live-editor).