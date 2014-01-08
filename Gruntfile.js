module.exports = function(grunt) {
  "use strict";

  grunt.initConfig({

    // Metadata.
    pkg: grunt.file.readJSON('package.json'),
    banner:
      '/*!\n' +
      ' * <%= pkg.name %> <%= pkg.version %> (<%= grunt.template.today("yyyy-mm-dd") %>)\n' +
      ' * Copyright 2013-2014 <%= pkg.author %>\n' +
      // TODO: Specify License
      // ' * License: <%= pkg.license %>\n' +
      ' * ---\n' +
      ' * JavaScript BigInteger library version 0.9\n' +
      ' * http://silentmatt.com/biginteger/\n' +
      ' * \n' +
      ' * Copyright (c) 2009 Matthew Crumley <email@matthewcrumley.com>\n' +
      ' * Copyright (c) 2010,2011 by John Tobey <John.Tobey@gmail.com>\n' +
      ' * Licensed under the MIT license.\n' +
      ' * \n' +
      ' * Support for arbitrary internal representation base was added by Vitaly Magerya.\n' +
      ' * ---\n' +
      ' * A JavaScript implementation of the Secure Hash Algorithm, SHA-256, as defined\n' +
      ' * in FIPS 180-2\n' +
      ' * Version 2.2 Copyright Angel Marin, Paul Johnston 2000 - 2009.\n' +
      ' * Other contributors: Greg Holt, Andrew Kepert, Ydnar, Lostinet\n' +
      ' * Distributed under the BSD License\n' +
      ' * ---\n' +
      ' * jsBCrypt is an implementation of BCrypt written in JavaScript.\n' +
      ' * It uses Components of the ISAAC.\n' +
      ' * It is based upon jBCrypt\n' +
      ' * Licence: http://opensource.org/licenses/BSD-3-Clause\n' +
      ' * ---\n' +
      ' * A JavaScript implementation of the RSA Data Security, Inc. MD5 Message\n' +
      ' * Digest Algorithm, as defined in RFC 1321.\n' +
      ' * Version 2.2 Copyright (C) Paul Johnston 1999 - 2009\n' +
      ' * Other contributors: Greg Holt, Andrew Kepert, Ydnar, Lostinet\n' +
      ' * Distributed under the BSD License\n' +
      ' * See http://pajhome.org.uk/crypt/md5 for more info.\n' +
      ' */\n\n',

    // Task configuration.
    clean: {
      dist: ['dist']
    },

    concat: {
      options: {
        banner: '<%= banner %>',
        stripBanners: true
      },
      dist: {
        src: [
          'src/prefix.js',
          'bcrypt/bCrypt.js',
          'bigint/biginteger.js',
          'hmac/md5.js',
          'src/suffix.js'
        ],
        dest: '<%= pkg.name %>.js'
      }
    },

    uglify: {
      options: {
        banner: '<%= banner %>',
        report: 'min'
      },
      dist: {
        src: ['<%= concat.dist.dest %>'],
        dest: '<%= pkg.name %>.min.js'
      }
    }
  });

  // These plugins provide necessary tasks.
  grunt.loadNpmTasks('grunt-contrib-clean');
  grunt.loadNpmTasks('grunt-contrib-concat');
  grunt.loadNpmTasks('grunt-contrib-uglify');

  // Default task.
  grunt.registerTask('default', ['concat', 'uglify']);
};
