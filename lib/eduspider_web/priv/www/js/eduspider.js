function navToSearch() {
    var P = $('#searchtext').val();
    window.location.href = '/wiki/'+encodeURIComponent(P);
}

function articleURL() {
    L = window.location.href;
    return L.slice(0, L.indexOf('?'));
}

function decode_utf8(string) {
    return decodeURIComponent(escape(string));
}

function readCookie(name) {
    var nameEQ = name + "=";
    var ca = document.cookie.split(';');
    for(var i=0;i < ca.length;i++) {
        var c = ca[i];
        while (c.charAt(0)==' ') c = c.substring(1,c.length);
        if (c.indexOf(nameEQ) == 0)
            return decode_utf8(c.substring(nameEQ.length,c.length));
    }
    return null;
}

function clearCookie(name) {
    document.cookie = name + '=; path=/; expires=Thu, 01-Jan-70 00:00:01 GMT;';
}

function loginSignupSuccess() {
    if (window.location.href.indexOf('next=') > 0) {
        start = window.location.href.indexOf('next=')+('next='.length);
        end = window.location.href.indexOf('&', start)
        if (end < 0) end = window.location.href.length;
        window.location.href =
            decodeURIComponent(
                window.location.href.slice(start, end));
    } else {
        window.location.href = '/';
    }
}

function submitLogin() {
    var username = $('input[name=login_username]').val();
    var password = $('input[name=login_password]').val();
    
    $.ajax({
        url:'/user/'+username,
        type:'POST',
        data:{'password':password},
        success:loginSignupSuccess,
        error:function(req) {
            $('#loginerror').text('incorrect username/password combination');
        }
    });
}

function submitSettings() {
    var data = {};

    var p = $('input[name=password]').val();
    if (p) data.password = p;
    
    var e = $('input[name=email]').val();
    if (e) data.email = e;

    var b = $('input[name=bio]').val();
    if (b) data.bio = b;

    var u = $('input[name=username]');
    if (u.length) {
        data.username = u.val();
        if (!data.username) {
            alert("Please choose a username.");
            return;
        }
        if (!data.password) {
            alert("Please choose a password.");
            return;
        }
    }

    req = {
        url: data.username ? '/user/'+data.username : window.location.href,
        type: 'PUT',
        data: data,
        success: loginSignupSuccess,
        error: function(req) {
            if (req.status == 409)
                $('#settingserror').text('the requested username is taken');
            else
                $('#settingserror').text('an unknown error occured: '+req.responseText);
        }
    };
    $.ajax(req);
}

function onEnter(handler) {
    return function(e) {
        if (e.keyCode == 10 || e.keyCode == 13)
            handler();
    };
}

$(function() {
    /* Header search buttons */
    $('#searchbutton').click(navToSearch);
    $('#searchtext').keyup(onEnter(navToSearch));

    /* Article editor buttons */
    $('#editcancel').click(function() {
        window.location.href = articleURL();
    });

    $('#editsave').click(function() {
        var req = {
            url: articleURL(),
            type: 'PUT',
            data: {
                text:$('#edittext').val(),
                msg:$('#editmsg').val(),
                vclock:$('#editvclock').val()
            },
            success: function() { window.location.href = req.url; }
        };
        $.ajax(req);
    });

    /* User settings buttons */
    $('#settingsave').click(submitSettings);
    $('.settingfield').keyup(onEnter(submitSettings));

    $('#loginbutton').click(submitLogin);
    $('.loginfield').keyup(onEnter(submitLogin));
    
    $('#logoutbutton').click(function() {
        
        $.ajax({
            url:'/user/'+readCookie('username')+'/'+readCookie('session'),
            type:'DELETE',
            success:function() { window.location.href = '/'; },
            error:function(req) {
                if (req.status == 404) //already logged out
                    window.location.href = '/';
            }
        });
    });

    /* Logout and refresh the current page */
    /*
    $("#logout_anchor").click(function() {
        $.post("/logout", function() {
            location.reload();
        });
    });
    */
    if (readCookie('username') && readCookie('session')) {
        $.ajax({
            url:'/user/'+readCookie('username')+'/'+readCookie('session'),
            success:function() {
                $('#login').hide();
                $('#welcome')
                    .find('a').text(decodeURIComponent(readCookie('username')))
                    .attr('href', '/user/'+readCookie('username')+'?edit')
                    .end()
                    .css('display', 'block');
            },
            error:function(req) {
                if (req.status == 404) {
                    clearCookie('username');
                    clearCookie('session');
                }
            }
        });
    }

    /* following are the backbone related stuff */

    /*backbone js starts from here*/
    var router = null;

    var LoginView = Backbone.View.extend({
        render: function() {
            var html = router.renderT('login', {})
            $(this.el).html(html);
            return this.el;
        }
    });

    window.EduSpiderRouter = Backbone.Router.extend({
        routes: {
            'login': 'login'
        },

        tcache: {},

        setView: function(view, title) {
            var html = view.render();
            console.log("setview html:"+html);
            $('#content').html(html);

            var newTitle = "EduSpider"
            if(!!title && title.length > 0) {
                newTitle += " - " + title;
            }
            document.title = newTitle;
            window.prettyPrint && prettyPrint();
        },

        loadT: function(name) {
            var url = "/static/views/" + name + ".handlebars";
            var template = $.ajax({url: url, async: false}).responseText;
            console.log("template:"+template);
            return Handlebars.compile(template);
        },

        renderT: function(name, data) {
            var self = this;
            console.log("before loading tcache:"+self.tcache[name]);
            if(!self.tcache[name]) {
                self.tcache[name] = self.loadT(name);
            }
            console.log("after loading tcache:"+self.tcache[name]);
            return self.tcache[name](data || {});
        },

        login: function() {
            var view = new LoginView();
            this.setView(view, "Login");
        },

        logout: function() {
            $.post("/logout", function() {
                location.reload();
            });
        },

        start: function(user_name) {
            this.uid = user_name;

            console.log("user_name:"+user_name);
            console.log("this.uid:"+this.uid);
            router = this;

            Backbone.history.start();

            $("#logout_anchor").click(function(e) {
                e.preventDefault();
                router.logout();
            });

            var hash = window.location.hash;
            console.log("hash:"+hash);
            if(hash === '' || hash === '#') {
                console.log("here1, user_name:"+user_name);
                if(user_name) {
                    this.navigate('login', {trigger: true, replace: true});
                } else {
                    console.log("here3");
                    //this.navigate('user/' + userId, {trigger: true, replace: true});
                }
            }
        }
    });
});
