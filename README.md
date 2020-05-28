# HLola

**HLola** is an implementation of the Stream Runtime Verification language [Lola](http://software.imdea.org/~cesar/papers/2005/time05/time05.pdf) as en embedded Domain Specific Language over Haskell.
In this repository, you will find the source code of **HLola**, along with the steps to use the engine and define new specifications, shown in the following sections.

## Docker image

We provide a [Docker image](https://hub.docker.com/r/imdeasoftware/hlola) ready to build and execute **HLola** specifications out of the box.

You can run the image and provide arguments `arg0`, `arg1`, `arg2` for the binary `HLola` in the docker image by running `docker run imdeasoftware/hlola arg0 arg1 arg2`.
If you choose to use the Docker image to try **HLola**, you can skip the following section.

## Building Process

To build the project, you will need to install the Haskell [Stack](https://docs.haskellstack.org/en/stable/README/) tool.

Then, you should run the following commands to build the entire project:

```bash
$> stack setup # which will set up all the environment needed to build HLola
$> stack install # which will build and install HLola
```

The resulting binary will be placed in the directory output by the following command:
```bash
$> stack path --local-bin
```

Make sure that the previous path is included in your PATH, so you can execute **HLola** by typing `$> HLola` in a shell.

## Definition of a new specification

To define a new specification, we need to create a Haskell file in the folder `src/Specs`. For example, we can name our specification `MySpec` and thus the path f the specification will be `src/Specs/MySpec.hs`.
See the example specifications in `src/Examples` to get insight on the general shape of an **HLola** specification.
This specification must declare a varible of type `Specification`, for example, let's say we define a `spec :: Specification`.
Then, we modify the main file at `app/Main.hs` to import our new specification using `import qualified Specs.MySpec as MySpec(spec)`.
Finally, we bind the variable `importedSpec` in `Main` to the imported specification: `importedSpec = MySpec.spec`.

## HLola execution

If you execute `HLola` without arguments, you will be prompted with a help screen:
```bash
$> HLola
Wrong arguments. Usage:
  HLola --analyse
  HLola --execute
  HLola QuickCheck
  HLola TestType backref tracelen
Modify Main.hs to specify the imported the spec to --analyse or --execute.
TestType must be one of (PeriodWidth | PeriodHeight | SmoothPeriodWidth | SmoothPeriodHeight | WindowTrueWidth | WindowTrueHeight), while backref and tracelen must be positive natural numbers.
$>
```

Analyse or execute the specification imported in Main.hs using `HLola --analyse` or `HLola --execute`.
Alternatively, **HLola** ships with some predefined specifications for testing the tool.
To see the use of QuickCheck over a spec in action, execute **HLola** using `HLola QuickCheck`.
You can execute a specification `spec` from the specifications shown in the paper under submission to [PPDP 2020](http://www.cse.chalmers.se/~abela/ppdp20/) to stress the monitor by choosing a trace length `tracelen` and a back reference `backref`, and then executing `HLola TestType backref tracelen`.

In the folder `empirical`, you will find the binaries to replicate the three experiments shown in the paper.
If you are working on the Docker image, the binaries are ready to execute, but if you have downloaded the sources to your machine, make sure to `stack install` **HLola** before running the experiments.
