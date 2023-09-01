# CamlWriter

The **CamlWriter** project is a terminal-based text editor that has been developed using the OCaml programming language. 

Key features of the CamlWriter:

1. _Terminal Interface_: Caml Writer is designed to be used entirely within a terminal environment, making it suitable for users who prefer working with text editors from the command line.

2. _Basic Text Editing_: Users can create, open, edit, and save plain text files using standard text editing commands. This includes functionalities like inserting, deleting, copying, and pasting text.

## Project Structure

The project structure is organized as follows:

```
.
├── bin
│   ├── dune
│   ├── main.ml
│   └── ...
├── dune-project
├── lib
│   └── dune
├── caml_writer.opam
└── test
    ├── dune
    └── caml_writer.ml```
```

- `./bin/main.ml`: This file contain the entry point of **CamlWriter**.

- `./bin/`: This folder is intended for executable code.

- `./lib/`: This folder is intended for the libraries of **CamlWriter**.

- `./test/`: This folder is intended for test-related code.

## Setup ocaml

1. **Install opam:** It's easy to install opam with your system's package manager on Linux:
   ```bash
   # Ubuntu and Debian:
   $ apt install opam

   # Archlinux
   $ pacman -S opam
   ```

2. **Initialize opam** and **Create an opam switch**:
    ```bash
    $ opam init          # Can take some time
    $ eval $(opam env)
    ```
    ```
    $ opam switch create 4.14.0
    $ eval $(opam env)
    ```

3. **OCaml Platform Tools on Unix**: All these tools can be installed in your current switch:
    ```bash
    $ opam install dune merlin ocaml-lsp-server odoc ocamlformat utop dune-release
    ```

4. **Install Dependencies**: 
    ```bash
    $ opam install unix 
    ```

## Getting Started

1. **Clone the Repository:** Clone this repository to your local machine using the following command:

   ```
   https://github.com/FedericoBruzzone/setup-ocaml-project-without-dune.git
   ```

## Getting Started

1. **Clone the Repository:** Clone this repository to your local machine using the following command:

   ```
   https://github.com/FedericoBruzzone/caml-writer.git
   ```

2. **Compile and Run:**
    
    - To compile the code and create an executable with `make`, run:
      ```bash
      $ make        # or make build
      $ make run
      ```

      eventually,

      ```bash
      $ make clean
      ```

    - To compile the code and create an executable with `dune`, run:
        ```bash
        $ dune build 
        $ dune exec caml_writer
        ```

        eventually,

        ```bash
        $ dune clean
        ```

## Useful command

`ocamlfind list`

`opam list`

## Contributing

Contributions to this project are welcome! If you have any suggestions, improvements, or bug fixes, feel free to submit a pull request.

## License

This repository is licensed under the [GNU General Public License (GPL)](https://www.gnu.org/licenses/gpl-3.0.html). Please review the license file provided in the repository for more information regarding the terms and conditions of the GPL license.

## Contact

If you have any questions or suggestions regarding this repository, please don't hesitate to reach out. You can contact us via the GitHub repository or through the following channels:
- Email: [federico.bruzzone.i@gmail.com] or [federico.bruzzone@studenti.unimi.it]

---
