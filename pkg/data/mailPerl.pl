# load or install required modules
system "ppm install Net::SMTP" unless( eval{require Net::SMTP} );
die "Module Net::SMTP is missing!" unless( eval{require Net::SMTP} );

# load or install required modules
system "ppm install Authen::SASL" unless( eval{require Authen::SASL} );
die "Module Authen::SASL is missing!" unless( eval{require Authen::SASL} );

 use Net::SMTP; 
# use Net::SMTP_auth;
 use Authen::SASL;
 #use Mime::lite
 sub send_mail{
  my($user,$pass,$server,$to,$from,$subject,@body) = @_;
  my $smtp_host = $server;# This is your SMTP server
  my $smtp = Net::SMTP->new(
   Host   => $smtp_host,
   Hello  => $smtp_host,
   Timeout=> 30,
   Debug  => 1);

  $smtp->auth($user, $pass); #Replace "username" and "pass" will your own ones
  die "Could not open connection:$!" if ( !defined $smtp );
  
  $smtp->mail($from);
  $smtp->to($to);
  
  $smtp->data();
  $smtp->datasend("From:$from\n");
  $smtp->datasend("Subject:$subject\n");
  $smtp->datasend("\n");
  
  foreach (@body){
   $smtp->datasend("$_\n");
  }
 
  $smtp->dataend();
  $smtp->quit;
 }
 
 #DropZone
send_mail("user","pass","server","recipient","sender","Title","message")
