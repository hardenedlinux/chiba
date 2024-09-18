<!DOCTYPE html>
<html>
    <@include header.html.tpl %>
    <meta name="referrer" content="strict-origin-when-cross-origin">
    <script>
     $("#reset").on("click", function() {
         $('label').removeClass('active');
     });
    </script>
    </head>

    <body>
        <div class="container">
            <div class="row">
                <div class="col-md-6 col-md-offset-3">
                    <form class="form-horizontal" action="login.php" method="post">
                        <fieldset>
                            <legend>Login</legend>
                            <div class="form-group>
                                        <label for="username" class="control-label">Username</label>
                                <input type="text" class="form-control" id="username" name="username" placeholder="Username">
                            </div>
                            <div class="form-group">
                                <label for="password" class="control-label">Password</label>
                                <input type="password" class="form-control" id="password" name="password" placeholder="Password">
    </body>
</html>
