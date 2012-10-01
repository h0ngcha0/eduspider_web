/*
 * -------------------------------------------------------------------
 *
 * Copyright (c) 2009-2010 Basho Technologies, Inc.  All Rights Reserved.
 *
 * This file is provided to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License.  You may obtain
 * a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 * -------------------------------------------------------------------
 */
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
});
