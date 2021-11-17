import React from "react";

const EtsTables = (props) => {
    // console.log(props.tables);
    return (
        <div>
            <button>hide tables</button>
            <table>
                <thead>
                    <tr>
                        <th>&nbsp;</th>
                        <th>name</th>
                        <th>size</th>
                        <th>registered Name</th>
                        <th>compressed</th>
                        <th>heir</th>
                        <th>id</th>
                        <th>keypos</th>
                        <th>memory</th>
                        <th>named_table</th>
                        <th>node</th>
                        <th>owner</th>
                        <th>protection</th>
                        <th>type</th>
                        <th>read_concurrency</th>
                        <th>write_concurrency</th>
                        <th>decentralized_counters</th>
                    </tr>
                </thead>
                <tbody>
                {
                    props.tables.map((t) => (
                        <tr key={t.name}>
                            <td><button onClick={() => props.viewTable(t.name)}>View</button></td>
                            <td>{t.name}</td>
                            <td>{t.size}</td>
                            <td>{t.reg_name}</td>
                            <td>{''+t.compressed}</td>
                            <td>{t.heir}</td>
                            <td>{t.id}</td>
                            <td>{t.keypos}</td>
                            <td>{t.memory}</td>
                            <td>{''+t.named_table}</td>
                            <td>{t.node}</td>
                            <td>{t.owner}</td>
                            <td>{t.protection}</td>
                            <td>{t.type}</td>
                            <td>{''+t.read_concurrency}</td>
                            <td>{''+t.write_concurrency}</td>
                            <td>{''+t.decentralized_counters}</td>
                        </tr>
                    ))
                }
                </tbody>
        </table>
    </div>
    );
}

export default EtsTables
