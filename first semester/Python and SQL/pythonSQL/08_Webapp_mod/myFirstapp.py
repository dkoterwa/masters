from bottle import Bottle, route, run, template, get, post, debug, static_file, request, redirect, response

@route('/static/:path#.+#', name='static')
def static(path):
    return static_file(path, root='./static')

@route('/hello/<name>')
@route('/hello/<name>/')
@route('/hello/')
@route('/hello')
def index(name="Anonymous"):
    return template('<b>Hello {{name}}</b>!', name=name)

@route('/contact/')
@route('/contact')
def contact():
    mess =  '''<center>
    <br><br><br><br>
    This is a contact information:<br>
    Address: <b>Dluga 44 , 00-456 Warszawa</b><br>
    Tel: <b>555- 555 --55</b><br>
    </center>
    '''
    return template('index2', message=mess, loginName="Maciej")

@route('/news/')
@route('/news')
def news():
    news = [
("02.12.19", "Python is awsome", "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ips"),
("01.12.19", "Web apps are easy", "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but as"),
("30.11.19", "Datascience is thefuture", "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ips Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ips"),
]

    mess =  ""
    for date, title, text in news:
        mess += f'''
        <div class="col-md-6">
	<h4>{title}</h4>
	<p>{text}</p>
    Date: {date}
	</div>
        '''

    return template('index2', message=mess, loginName="Maciej")


@route('/news2/')
@route('/news2')
def news2():
    news = [
("02.12.19", "Python is awsome", "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ips"),
("01.12.19", "Web apps are easy", "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but as"),
("30.11.19", "Datascience is thefuture", "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ips Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ips"),
]



    return template('news', elements=news)

@route('/bye/<name>')
@route('/bye/')
@route('/bye')
def index(name="Anonymous"):
    return template('<b>Bye {{name}}</b>!', name=name)

@route('/')
def index(name="Maciej"):
    messDict = {'error': "Something went wrong",
                'ok': "Everything is ok.  "}
    return template('index', message=messDict.get("none", ""), loginName=name)

	
	
run(host='localhost', port=8001, debug=True, reloader=True)