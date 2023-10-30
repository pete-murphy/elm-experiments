module.exports = {
    content: [
        './index.html',
        './src/js/main.js',
        './src/css/main.css',
        './src/elm/**/*.elm'
    ],
    theme: {
        extend: {},
        fontFamily: {
            sans: ["Name Sans", "sans-serif"]
        }
    },
    plugins: [
        require('@tailwindcss/forms'),
        require('@tailwindcss/typography'),
        require('@tailwindcss/container-queries'),
    ],
}