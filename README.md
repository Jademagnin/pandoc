
# Pandoc project

Pandoc is a powerful, versatile open-source tool that is used for converting files from one markup format to another. It's often referred to as the "Swiss army knife" of document conversion because of its wide range of supported formats.

## Supported Format

The project supports two different format for the input and three in output : json and xml, and markdown, json, xml.

You are free to add the format you want. All you have to do is create a new file in input and output, and add the format on ArgsParser and on Display.

## Usage

Clone the project

```bash
  git clone git@github.com:Jademagnin/pandoc.git pandoc
```

Go to the project directory

```bash
  cd pandoc
```

Start the project

```bash
  make re
  ./mypandoc -i file_to_convert -f format_output -o (optional) output_path -e (optional) format_input
```

## Contributing

Contributions are always welcome!
If you encounter any bugs, have suggestions for improvements, or would like to contribute additional features, please feel free to submit a pull request.

## License

This project is licensed under the [MIT](https://choosealicense.com/licenses/mit/) License, which means you are free to use, modify, and distribute the code as long as you include the original copyright and license notice.