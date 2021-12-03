import React from 'react';
import EtsTableViewRow from './EtsTableViewRow'

const EtsTableViewRows = (props) => {
    console.log('EtsTableViewRows.props.rows '+props.rows);
    let rowcount = 0;
    if ( props.rows.length > 0 ) {
        return (
            <div id="erlang_entries_div">
                <pre>
                    <code className="Erlang" id="erlang_entries">
                        {
                            props.rows.map((entry) =>
                                (
                                    <EtsTableViewRow key={rowcount++} rowcount={rowcount++} values={entry.values} />
                                )
                            )
                        }
                    </code>
                </pre>
            </div>
        );
    } else {
        return '';
    }
}

export default EtsTableViewRows;