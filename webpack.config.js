const path = require('path');

module.exports = {
    mode: 'development',
    entry: './priv/src/app.js',
    output: {
        path: path.join(__dirname, 'priv/www'),
        filename: 'bundle.js'
    },
    module: {
        rules: [
            {
                test: /\.m?js$/,
                exclude: /node_modules/,
                use: {
                  loader: 'babel-loader',
                }
              }
        ]
    },
    devtool: 'cheap-module-eval-source-map',
    devServer: {
        contentBase: path.join(__dirname, 'priv/www')
    }
}