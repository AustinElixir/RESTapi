{application, simple, % replace simple with your application name.
             [{description,"An OTP Application"},
              {vsn,"0.1.0"},
              {registered,[]},
              {mod,{ simple_app ,[]}}, % application callback module
              {applications,[kernel,stdlib]},
              {env,[]},
              {modules,[ % list of modules supplied by this application
                simple_app,
                simple_gen,
                simple_sup
              ]},
              {contributors,[]},
              {licenses,[]},
              {links,[]}]}.