const colors = require('tailwindcss/colors');
const plugin = require('tailwindcss/plugin')

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
        '25': '.25',
        '75': '.75'
      },   
      grayscale: {
        '50': '50%',
      },
      spacing: {
        '-9999': '-9999px',
        '2px': '2px',
        '-2px': '-2px',
        '6ish': '1.56rem',
        '-6ish': '-1.56rem',
        '1.5ish': '0.4rem',
        '-1.5ish': '-0.4rem',
        '2ish': '0.56rem',
        '-2ish': '-0.56rem',
        '1/12': '8.333333%',
        '2/12': '16.666667%',
        '3/12': '25%',
        '4/12': '33.333333%',
        '5/12': '41.666667%',
        '6/12': '50%',
        '7/12': '58.333333%',
        '8/12': '66.666667%',
        '9/12': '75%',
        '10/12': '83.333333%',
        '11/12': '91.666667%',
        '1/15': '6.666667%',
        '2/15': '13.333333%',
        '3/15': '20%',
        '4/15': '26.666667%',
        '5/15': '33.333333%',
        '6/15': '40%',
        '7/15': '46.666667%',
        '8/15': '53.333333%',
        '9/15': '60%',
        '10/15': '66.666667%',
        '11/15': '73.333333%',
        '12/15': '80%',
        '13/15': '86.666667%',
        '14/15': '93.333333%'
      },
      skew: {
       '25': '25deg',
       '60': '60deg',
      },
      keyframes: {
        // wiggle: {
        //   '0%': { transform: 'rotate(var(--tw-rotate))' },
        //   '50%': { transform: 'rotate(var(--tw-rotate))' },
        //   '100%': { transform: 'rotate(calc(var(--tw-rotate) + 5deg))' }, // perspective(10rem) translateZ(1rem)' },
        // },
        // nonCurrentToNonCurrentHover: {
        //   '0%': { transform: 'perspective(50rem) translateZ(calc(var(--index, 0) * -1rem)) rotate(var(--rotate, 0))' },
        //   '100%': { transform: 'perspective(50rem) translateZ(calc(var(--index, 0) * -1rem)) translateX(1rem) translateY(-1rem) rotate(var(--rotate, 0))' }
        // },
        // nonCurrentHoverToNonCurrent: {
        //   '0%': { transform: 'perspective(50rem) translateZ(calc(var(--index, 0) * -1rem)) translateX(1rem) translateY(-1rem) rotate(var(--rotate, 0))' },
        //   '100%': { transform: 'perspective(50rem) translateZ(calc(var(--index, 0) * -1rem)) rotate(var(--rotate, 0))' }
        // }, 
        // nonCurrentHoverToCurrentHover: {
        //   '0%': { transform: 'perspective(50rem) translateZ(calc(var(--index, 0) * -1rem)) translateX(1rem) translateY(-1rem) rotate(var(--rotate, 0))' },
        //   '100%': { transform: 'perspective(50rem) translateZ(calc(var(--index, 0) * -1rem)) translateX(1rem) translateY(-1rem) rotate(var(--rotate, 0))' }
        // },
        // currentToCurrentHover: {
        //   '0%': { transform: 'perspective(50rem) translateZ(calc(var(--index, 0) * -1rem)) rotate(var(--rotate, 0))' },
        //   '100%': { transform: 'perspective(50rem) translateZ(calc(var(--index, 0) * -1rem)) translateX(1rem) translateY(-1rem) rotate(var(--rotate, 0))' }
        // },
        // currentHoverToCurrent: {
        //   '0%': { transform: 'perspective(50rem) translateZ(calc(var(--index, 0) * -1rem)) translateX(1rem) translateY(-1rem) rotate(var(--rotate, 0))' },
        //   '100%': { transform: 'perspective(50rem) translateZ(calc(var(--index, 0) * -1rem)) rotate(var(--rotate, 0))' },
        // }
      },
      animation: {
        // wiggle: 'wiggle 0.25s ease-in-out 1 forwards',
        // nonCurrentToNonCurrentHover: 'nonCurrentToNonCurrentHover 0.25s ease-in-out 1 forwards',
        // nonCurrentHoverToNonCurrent: 'nonCurrentHoverToNonCurrent 0.25s ease-in-out 1 forwards',
        // nonCurrentHoverToCurrentHover: 'nonCurrentHoverToCurrentHover 0.25s ease-in-out 1 forwards',
        // currentToCurrentHover: 'currentToCurrentHover 0.25s ease-in-out 1 forwards',
        // currentHoverToCurrent: 'currentHoverToCurrent 0.25s ease-in-out 1 forwards'
      },
    },

    screens: {
      'xs': '500px',
      // => @media (min-width: 500px) { ... }
      'sm': '640px',
      // => @media (min-width: 640px) { ... }

      'md': '768px',
      // => @media (min-width: 768px) { ... }

      'lg': '1024px',
      // => @media (min-width: 1024px) { ... }

      'xl': '1280px',
      // => @media (min-width: 1280px) { ... }

      '2xl': '1536px',
      // => @media (min-width: 1536px) { ... }
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
      '-3': '-3px 0 10px 0px rgba(0, 0, 0, 0.5)',
      '3': '3px 0 10px 0px rgba(0, 0, 0, 0.5)',
      '-10': '-10px 0 10px 0px rgba(0, 0, 0, 0.5)',
      '10': '10px 0 10px 0px rgba(0, 0, 0, 0.5)',
      'white': '0 1px 3px 0 rgba(255, 255, 255, 0.1), 0 1px 2px 0 rgba(255, 255, 255, 0.06)',

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
      // margin: ['first', 'last'],
      // padding: ['first', 'last'],
      // borderRadius: ['first', 'last'],
      backgroundColor: ['focus', 'group-hover', 'hover'],
      // backgroundOpacity: ['disabled'],
      animation: ['hover'],
    }
  },
  plugins: [
    plugin(function({ addUtilities }) {
      addUtilities({
        '.text-sideways': {
          'writing-mode': 'vertical-rl',
          'text-orientation': 'sideways',
        }
      }, {
        variants: ['responsive']
      })
    })
  ],
}
