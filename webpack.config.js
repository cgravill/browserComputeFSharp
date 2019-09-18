// Note this only includes basic configuration for development mode.
// For a more comprehensive configuration check:
// https://github.com/fable-compiler/webpack-config-template

var path = require("path");
var webpack = require("webpack");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const MonacoWebpackPlugin = require('monaco-editor-webpack-plugin');

const APP_DIR = path.resolve(__dirname, './src');
const MONACO_DIR = path.resolve(__dirname, './node_modules/monaco-editor');

var isProduction = !process.argv.find(v => v.indexOf('webpack-dev-server') !== -1);

var CONFIG = {
    babel: {
        presets: [
            "@babel/preset-react",
            // ["@babel/preset-env", {
            //     "targets": {
            //         "browsers": ["last 2 versions"]
            //     },
            //     "modules": false
            // }]
        ],
        plugins: [
            "@babel/plugin-proposal-class-properties",
            "@babel/plugin-syntax-import-meta"
        ]
    }
}

module.exports = {
    mode: "development",
    /*externals:{
        fs:    "commonjs fs",
        path:  "commonjs path"
    },*/
    entry: ["./src/App.fsproj", './src/scss/main.scss'],
    output: {
        path: path.join(__dirname, "./public"),
        filename: "bundle.js",
    },
    devServer: {
        contentBase: "./public",
        port: 8080,
        hot: true,
        inline: true
    },
    module: {
        rules: [/*{
            test: /\.wasm$/,
            type: "webassembly/experimental"
        },*/ {
            test: /\.fs(x|proj)?$/,
            use: "fable-loader"
        },
        {
            test: /\.js$/,
            exclude: /node_modules/,
            use: {
                loader: 'babel-loader',
                options: CONFIG.babel
            },
        },
        {
            test: /\.(sass|scss|css)$/,
            include: APP_DIR,
            use: [
                isProduction
                    ? MiniCssExtractPlugin.loader
                    : 'style-loader',
                'css-loader',
                'sass-loader',
            ],
        },
        {
            test: /\.css$/,
            include: APP_DIR,
            use: ['style-loader', 'css-loader']
        },
        {
            test: /\.css$/,
            include: MONACO_DIR,
            use: ['style-loader', 'css-loader'],
        },
        {
            test: /\.(png|jpg|jpeg|gif|svg|woff|woff2|ttf|eot)(\?.*$|$)/,
            use: ["file-loader"]
        }]
    },
    plugins : isProduction ? [] : [
        new webpack.HotModuleReplacementPlugin(),
        new MonacoWebpackPlugin({
            languages: [
                "fsharp",
                "html",
                "css",
                "cpp",
                "javascript",
                "csharp",
                // We need typescript too, see https://github.com/Microsoft/monaco-editor-webpack-plugin/issues/27
                "typescript"
            ],
            features: [
                'accessibilityHelp',
                'bracketMatching',
                'caretOperations',
                'clipboard',
                'codelens',
                'colorDetector',
                'comment',
                'contextmenu',
                // 'coreCommands',
                'cursorUndo',
                // 'dnd',
                'find',
                // 'folding',
                // 'format',
                'goToDefinitionCommands',
                'goToDefinitionMouse',
                'gotoError',
                'gotoLine',
                'hover',
                'inPlaceReplace',
                'inspectTokens',
                // 'iPadShowKeyboard',
                'linesOperations',
                'links',
                'multicursor',
                'parameterHints',
                // 'quickCommand',
                // 'quickFixCommands',
                // 'quickOutline',
                // 'referenceSearch',
                // 'rename',
                'smartSelect',
                // 'snippets',
                'suggest',
                'toggleHighContrast',
                'toggleTabFocusMode',
                'transpose',
                'wordHighlighter',
                'wordOperations'
            ]
        })
    ]
}