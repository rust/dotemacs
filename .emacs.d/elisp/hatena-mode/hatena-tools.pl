#!/usr/bin/perl -w

use strict;
use XMLRPC::Lite;
use Jcode;
use LWP::Simple;
use HTML::Parser;

my $rpc = XMLRPC::Lite->new;
$rpc->proxy('http://d.hatena.ne.jp/xmlrpc');

my $body = '*[emacs] hatena-mode

perl���Ѵ��黻�ҤȤ����Τ�����Τ��Τ�ʤ��ä���

*1128958699*[math] WS�ͥåȥ�����ո��ӥɥ�ե�

-�����餷�ϡ������Τ�꤯�꤬���Ѥ����顢"�������褦"�Ⱥ����⡢���ܤκ������äƤ����ơ����������ǽ���Τ����Ρ����ʤ���ͥåȥ��Ū�˶ᤤ��Τ��ո��Ӥˤʤ롣
-�����Ǥ����ͥåȥ���Ȥϡ������Ρ��ɤȤ���"Ĵ��"���뤳�Ȥˤ�ä�¿�����ͥåȥ���δ֤��ư����褦�ʡ�"ͼ�Ӥ��礦�ʤˤ��褦����¿�إͥåȥ����ǥ�"�Ǥ��롣
-�ո��Ӥ�"�쥷��"���꤭�ǤϤʤ����μ�����¢�ˤ���Ȥ��۵������Τʤ顢����äȰ�ä���ǥ벽������롣
-��¢�ˤ�IT������������IT���פȰ��̤ˤȤ館��ʤ顢��̳�����ѡ��ξ�����ꤤ����㤤��Τˤ������ؤλ����Ȥ����������������Ǥ��Τ���������Ū�Ϻ�Ŭ���ǡ����������׹ͤϤ��Ȱ���Ū����"iPod��podcasting���ƥ饸������ⲻ���ʲ��ھ�������롣"�Ȥ���"���줫��Υ����������Ω���ͤ��ơ������ˤϽ��ʤ���"�Ȥ���"���͹�ضɤ��¤�����⡢��񤷤��٤���"�Ȥ������������������������褬���ޤ��������Ȥ���"�η�"���ȯ���Ȥ��Ƥ��ʤ���Фʤ�ʤ���
';

my $res = $rpc->call(
  'hatena.setKeywordLink',
  {
    body => XMLRPC::Data->type('string',Jcode->new($body,'euc')->utf8),
    score => 20,
    cname => ['book','movie'],
    a_target => '_blank',
    a_class => 'keyword',
  }
);
if (my $fault = $res->fault){
  for (keys %{$fault}){
    warn $_."=>".$fault->{$_};
  }
} else {
  $body = $res->result;
  $body =~ s/&lt;/</ig;
  $body =~ s/&gt;/>/ig;
  $body =~ s/&quote;/"/ig;
  $body = Jcode->new($body,'utf8')->euc;
}


my @member = ();

while($body =~ m/(http:\/\/d\.hatena\.ne\.jp[^">]+)/g){
@member = (@member, $&);
    }

#������ɤΥڡ������顢�ȤäƤ��ơ���������Ǥ�
#$body ���� �������URL���äƤ��ơ��ä���
my $num = @member;
my $targeturl = get($member[int(rand $num)]);

my $num2 = 4;
my $p;
if($targeturl =~ m/<a href="(\/[a-zA-z0-9]+\/[0-9]+)">/){
    $p->parse( get('http://d.hatena.ne.jp'.$1) );
print $p;

}
