const colors = require('tailwindcss/colors');

module.exports = {
  purge: {
    content: [
      './client/*.html',
      './client/src/**/*.elm'
    ],
    options: {
      safelist: {
        greedy: [
          /^fill-/,
          /^stroke-/
        ]
      }
    }
  },
  darkMode: false, // or 'media' or 'class'
  theme: {
    extend: {
      brightness: {
        '25': '.25'
      }
    },

    colors: {
      // Build your palette here
      transparent: 'transparent',
      current: 'currentColor',
      black: colors.black,
      white: colors.white,
      gray: colors.blueGray,
      // lightGray: 'colors.coolGray.200',
      // lighterGray: 'colors.coolGray.100',
      red: colors.red,
      orange: colors.orange,
      amber: colors.amber,
      yellow: colors.yellow,
      lime: colors.lime,
      green: colors.green,
      emerald: colors.emerald,
      teal: colors.teal,
      cyan: colors.cyan,
      lightBlue: colors.lightBlue,
      blue: colors.blue,
      indigo: colors.indigo,
      violet: colors.violet,
      purple: colors.purple,
      fuchsia: colors.fuchsia,
      pink: colors.pink,
      rose: colors.rose,
    },
    stroke: theme => ({
      transparent: 'transparent',
      current: 'currentColor',
      'black': theme('colors.black'),
      'white': theme('colors.white'),
      'red': theme('colors.red.500'),
      'gray': theme('colors.gray.400'),
      'midGray': theme('colors.gray.300'),
      'lightGray': theme('colors.gray.200'),
      'lighterGray': theme('colors.gray.100'),
      'lightestGray': theme('colors.gray.50'),
      'red': theme('colors.red.500'),
      'orange': theme('colors.orange.500'),
      'amber': theme('colors.amber.500'),
      'yellow': theme('colors.yellow.500'),
      'lime': theme('colors.lime.500'),
      'green': theme('colors.green.500'),
      'emerald': theme('colors.emerald.500'),
      'teal': theme('colors.teal.500'),
      'cyan': theme('colors.cyan.500'),
      'lightBlue': theme('colors.lightBlue.500'),
      'blue': theme('colors.blue.500'),
      'indigo': theme('colors.indigo.500'),
      'violet': theme('colors.violet.500'),
      'purple': theme('colors.purple.500'),
      'fuchsia': theme('colors.fuchsia.500'),
      'pink': theme('colors.pink.500'),
      'rose': theme('colors.rose.500')
    }),
    fill: theme => ({
      transparent: 'transparent',
      current: 'currentColor',
      'black': theme('colors.black'),
      'white': theme('colors.white'),
      'red': theme('colors.red.500'),
      'gray': theme('colors.gray.400'),
      'midGray': theme('colors.gray.300'),
      'lightGray': theme('colors.gray.200'),
      'lighterGray': theme('colors.gray.100'),
      'lightestGray': theme('colors.gray.50'),
      'red': theme('colors.red.500'),
      'orange': theme('colors.orange.500'),
      'amber': theme('colors.amber.500'),
      'yellow': theme('colors.yellow.500'),
      'lime': theme('colors.lime.500'),
      'green': theme('colors.green.500'),
      'emerald': theme('colors.emerald.500'),
      'teal': theme('colors.teal.500'),
      'cyan': theme('colors.cyan.500'),
      'lightBlue': theme('colors.lightBlue.500'),
      'blue': theme('colors.blue.500'),
      'indigo': theme('colors.indigo.500'),
      'violet': theme('colors.violet.500'),
      'purple': theme('colors.purple.500'),
      'fuchsia': theme('colors.fuchsia.500'),
      'pink': theme('colors.pink.500'),
      'rose': theme('colors.rose.500')
    }),

    boxShadow: {
      sm: '0 1px 2px 0 rgba(0, 0, 0, 0.05)',
      DEFAULT: '0 1px 3px 0 rgba(0, 0, 0, 0.1), 0 1px 2px 0 rgba(0, 0, 0, 0.06)',
      md: '0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06)',
      lg: '0 10px 15px -3px rgba(0, 0, 0, 0.1), 0 4px 6px -2px rgba(0, 0, 0, 0.05)',
      xl: '0 20px 25px -5px rgba(0, 0, 0, 0.1), 0 10px 10px -5px rgba(0, 0, 0, 0.04)',
      '2xl': '0 25px 50px -12px rgba(0, 0, 0, 0.25)',
      inner: 'inset 0 2px 4px 0 rgba(0, 0, 0, 0.06)',
      none: 'none',
      'inner-t-md': 'inset 0 5px 5px 0 rgba(0, 0, 0, 0.1)',
      'inner-t-sm': 'inset 0 2px 2px 0 rgba(0, 0, 0, 0.1)',
      'inner-b-md': 'inset 0 -5px 5px 0 rgba(0, 0, 0, 0.1)',
      'inner-b-sm': 'inset 0 -2px 2px 0 rgba(0, 0, 0, 0.1)',
    },
    fontSize: {
      '3xs': '.5rem',
      '2xs': '.625rem',
      'xs': '.75rem',
      'sm': '.875rem',
      'base': '1rem',
      'lg': '1.125rem',
      'xl': '1.25rem',
      '2xl': '1.5rem',
      '3xl': '1.875rem',
      '4xl': '2.25rem',
      '5xl': '3rem',
      '6xl': '3.75rem',
      '7xl': '4.5rem',
      '8xl': '6rem'
    },
    minWidth: {
      '0': '0',
      '44': '11rem',
      'full': '100%',
    },
    minHeight: {
      '96': '24rem'
    }
  },
  variants: {
    extend: {
      margin: ['first', 'last'],
      padding: ['first', 'last'],
      borderRadius: ['first', 'last'],
      backgroundColor: ['focus', 'group-hover', 'hover'],
      backgroundOpacity: ['disabled'],
    }
  },
  plugins: [],
}
