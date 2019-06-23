import React from "react";
import { Container } from "../Components/Container";
import { Link } from "react-router-dom";
import { webSocket } from "rxjs/webSocket";

export class ChannelPage extends React.Component {
  componentDidMount() {
    webSocket("http://hurry.feblr.org/websocket").subscribe(data => {
      console.log(data);
    });
  }

  render() {
    return (
      <div className="page page--channel">
        <header>
          <Container className={["flex__box", "flex__box--vc"]}>
            <h3 className="flex__item">
              <Link to="/">Home</Link>
            </h3>
          </Container>
        </header>
        <section>
          <Container>
            <p>Channel Page</p>
          </Container>
        </section>
      </div>
    );
  }
}
