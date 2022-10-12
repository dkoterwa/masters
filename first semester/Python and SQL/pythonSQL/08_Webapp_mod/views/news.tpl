% rebase('base.tpl', title='Python')
<div class="row">
	<div class="col-md-12">
	
    % for date, title, text in elements:
    <div class="col-md-6">
	<h4>{{title}}</h4>
	<p>{{text}}</p>
    Date: {{date}}
	</div>
    % end
	
	
	</div>
</div>
