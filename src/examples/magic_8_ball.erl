
-module(magic_8_ball).





-export([

    answer/0,
    answers/0,

    stylize/1,

    page/1

]).





answer() ->

    sc:random_from(answers()).





answers() ->

    [   { yes,   "It is certain"             },
        { yes,   "It is decidedly so"        },
        { yes,   "Without a doubt"           },
        { yes,   "Yes definitely"            },
        { yes,   "You may rely on it"        },

        { yes,   "As I see it yes"           },
        { yes,   "Most likely"               },
        { yes,   "Outlook good"              },
        { yes,   "Yes"                       },
        { yes,   "Signs point to yes"        },

        { maybe, "Reply hazy try again"      },
        { maybe, "Ask again later"           },
        { maybe, "Better not tell you now"   },
        { maybe, "Cannot predict now"        },
        { maybe, "Concentrate and ask again" },

        { no,    "Don't count on it"         },
        { no,    "My reply is no"            },
        { no,    "My sources say no"         },
        { no,    "Outlook not so good"       },
        { no,    "Very doubtful"             }
    ].





stylize({ yes,   A }) -> "<span style=\"color:#080; background-color: #efe; padding: 1em;\">" ++ A ++ "</span>";
stylize({ maybe, A }) -> "<span style=\"color:#555; background-color: #eee; padding: 1em;\">" ++ A ++ "</span>";
stylize({ no,    A }) -> "<span style=\"color:#800; background-color: #fee; padding: 1em;\">" ++ A ++ "</span>".





%%%%%%%%% 
%%
%%  @doc Returns a magic 8 ball decision page.
%%
%%
%%  This is a simplistic webserver which chooses from a random fixed list of strings and returns
%%  one, according to the game "Magic 8-Ball"
%%
%%  ```1> htstub:serve( fun hello_world_ws:page/1 ).
%%  <0.45.0>
%%
%%
%%  You can see the kinds of responses you should expect by calling the page generator directly.
%%
%%  ```2> magic_8_ball:page(a).
%%  "<!doctype html><html><body><span style=\"color:#080;\">As I see it yes</span></body></html>"
%%
%%  3> magic_8_ball:page(a).
%%  "<!doctype html><html><body><span style=\"color:#555;\">Concentrate and ask again</span></body></html>"
%%
%%  4> magic_8_ball:page(a).
%%  "<!doctype html><html><body><span style=\"color:#800;\">My sources say no</span></body></html>"'''


page( _ ) ->

    "<!doctype html><html><body>" ++ stylize(answer()) ++ "</body></html>".
