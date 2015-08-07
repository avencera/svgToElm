
module.exports = function(grunt) {

  grunt.initConfig({

    watch: {
      elm: {
        files: ["*.elm"],
        tasks: ["elm"]
      }
    },


    elm: {
      dev :{
        files: {
          "./app.js": "./app.elm"
        }
      }
    },

    browserSync: {
      dev: {
        bsFiles : {
          src : "app.js"
        },
          options: {
          watchTask: true,
          notify : true,
          port: 8000,
          server:{
            baseDir : "./",
            index : "index.html"
          }
        }
      }
    },

  });

  ["grunt-contrib-watch" , "grunt-elm" , "grunt-browser-sync"].forEach(function(plugin) {
    grunt.loadNpmTasks(plugin);
  });

  grunt.registerTask("default", [ "elm", "browserSync:dev", "watch"]);
};