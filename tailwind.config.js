/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./webserver/src/**/*.{html,js}"],
  theme: {
    extend: {},
  },
  plugins: [
    require("@catppuccin/tailwindcss")({
      // which flavour of colours to use by default, in the `:root`
      defaultFlavour: "macchiato",
    }),
  ],
}

